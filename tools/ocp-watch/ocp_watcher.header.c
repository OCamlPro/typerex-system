/***********************************************************************/
/*                                                                     */
/*                             ocp-watch                               */
/*                                                                     */
/*  Copyright 2012 OCamlPro SAS                                        */
/*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    */
/*                                                                     */
/***********************************************************************/

#define _GNU_SOURCE
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <syslog.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <dlfcn.h>
#include <dirent.h>
#include <sys/types.h>
#include <utime.h>
#include <sys/time.h>


/* for msgsnd */
#include <sys/ipc.h>
#include <sys/msg.h>

#define PROTOCOL_P2W_KillMsg       0
#define PROTOCOL_P2W_ProcMsg       1
#define PROTOCOL_P2W_BeforeCallMsg 2
#define PROTOCOL_P2W_AfterCallMsg  3

#define PROTOCOL_W2P_BitmapMsg     0
#define PROTOCOL_W2P_ContinueMsg   1

static int pid = -1;
static int ppid = -1;
static int mq_P2W = -1;
static int mq_W2P = -1;
static char* function_bitmap = NULL;
static int nfunctions = 0;
static char function_digest[16];
static int option_flags = 0;

static void init_functions();
static FILE * (*libc_fopen) ( const char *, const char * );
static int (*libc_fclose) ( FILE * );
static char * (*libc_getenv) ( const char * );


static void finish(void)
{
}

/* sent to writer process on startup */
struct new_process_msg {
  long mtype;
  char cmd0;
  char cmd1;
  char pid0;
  char pid1;
  char ppid0;
  char ppid1;
};

/* received from writer process in reply to new_process_msg */
struct bitmap_msg {
  long mtype;
  char cmd0;
  char cmd1;
  char flags0;
  char flags1;
  char nfuns0;
  char nfuns1;
  char digest[16];
  char bitmap[1000];
};

/* received from writer process in reply to BeforeCallMsg in
   synchronous mode */
struct suspend_msg {
  long mtype;
  char flags0;
  char flags1;
  char nfuns0;
  char nfuns1;
  char digest[16];
  char bitmap[1000];
};

/* sent to writer process before a command */
struct call_msg {
  long mtype;
  char cmd0;
  char cmd1;
  char pid0;
  char pid1;
  char fun0;
  char fun1;
  char arg0;
  char arg1;
  char args[8000];
};

static int arg_pos = 0;
static struct call_msg call_msg;
static struct suspend_msg suspend_msg;

static void send_call(int mq, int cmd, int fun_id)
{
  int msg_res;

  call_msg.mtype = 1;
  call_msg.cmd0 = (cmd & 0xff);
  call_msg.cmd1 = ( (cmd >> 8) & 0xff);
  call_msg.pid0 = (pid & 0xff);
  call_msg.pid1 = ( (pid >> 8) & 0xff);
  call_msg.fun0 = (fun_id & 0xff);
  call_msg.fun1 = ( (fun_id >> 8) & 0xff);
  call_msg.arg0 = (arg_pos & 0xff);
  call_msg.arg1 = ( (arg_pos >> 8) & 0xff);
  msg_res = msgsnd(mq, &call_msg, sizeof(call_msg) - 8000 + arg_pos, 0);
  if( msg_res < 0 ){ perror("msgsnd"); exit(2);}
}

static void call_msg_init(void)
{
  arg_pos = 0;
}

#define ARG_STRING_NULL    0
#define ARG_STRING0        1
#define ARG_STRING0S_NULL  2
#define ARG_STRING0S       3
#define ARG_LONG           4
#define ARG_LONG_NEG       5
#define ARG_ULONG          6
#define ARG_FILEP_NULL     7
#define ARG_FILEP          8
#define ARG_DIRP_NULL      9
#define ARG_DIRP          10
#define ARG_STAT_NULL     11
#define ARG_STAT          12
#define ARG_DIRENT_NULL   13
#define ARG_DIRENT        14
#define ARG_ERRNO         15

static void call_msg_string0(const char *s)
{
  if( s == NULL) {
    call_msg.args[arg_pos++] = ARG_STRING_NULL;
  } else {
    int len = strlen(s);

    if(arg_pos + len > 7000){
      fprintf(stderr, "call_msg_string0: message too long\n");
      fprintf(stderr, "[%s]\n", s);
      exit(2);
    }
    call_msg.args[arg_pos++] = ARG_STRING0;
    call_msg.args[arg_pos++] = (len & 0xff);
    call_msg.args[arg_pos++] = ( (len >> 8) & 0xff);
    strncpy(call_msg.args+arg_pos, s, len);
    arg_pos += len;
  }
}

static void call_msg_stringsNULL(char * const *ss)
{
  if( ss == NULL){
    call_msg.args[arg_pos++] = ARG_STRING0S_NULL;
  } else {
    call_msg.args[arg_pos++] = ARG_STRING0S;
    while(*ss != NULL){
      call_msg_string0(*ss++);
    }
    call_msg_string0(NULL);
  }
}

static void call_msg_store_positive_long(long x)
{
 while(x > 0x7f){
    call_msg.args[arg_pos++] = x & 0x7f;
    x = x >> 7;
  }
  call_msg.args[arg_pos++] = x | 0x80;
}

static void call_msg_errno(int keep_errno)
{
  call_msg.args[arg_pos++] = ARG_ERRNO;
  call_msg_store_positive_long(keep_errno);
}

static void call_msg_long(long x)
{
  if(x < 0){
    call_msg.args[arg_pos++] = ARG_LONG_NEG;
    x = -x;
  } else {
    call_msg.args[arg_pos++] = ARG_LONG;
  }
  call_msg_store_positive_long(x);
}

static void call_msg_ulong(unsigned long x)
{
  call_msg.args[arg_pos++] = ARG_ULONG;
  call_msg_store_positive_long(x);
 }

static void call_msg_FILEP(FILE *f)
{
  if( f == NULL){
    call_msg.args[arg_pos++] = ARG_FILEP_NULL;
  } else {
    call_msg.args[arg_pos++] = ARG_FILEP;
    call_msg_store_positive_long(fileno(f));
  }
}

static void call_msg_DIRP(DIR *f)
{
  if( f == NULL){
    call_msg.args[arg_pos++] = ARG_DIRP_NULL;
  } else {
    call_msg.args[arg_pos++] = ARG_DIRP;
    call_msg_store_positive_long(dirfd(f));
  }
}

static void call_msg_stat(struct stat *st)
{
  if(st == NULL){
    call_msg.args[arg_pos++] = ARG_STAT_NULL;
  } else {
    call_msg.args[arg_pos++] = ARG_STAT;
    call_msg_store_positive_long(st->st_dev);
    call_msg_store_positive_long(st->st_ino);
    call_msg_store_positive_long(st->st_mode);
    call_msg_store_positive_long(st->st_size);
    call_msg_store_positive_long(st->st_uid);
    call_msg_store_positive_long(st->st_gid);
    call_msg_store_positive_long(st->st_mtime);
  }
}

static void call_msg_stat64(struct stat64 *st)
{
  if(st == NULL){
    call_msg.args[arg_pos++] = ARG_STAT_NULL;
  } else {
    call_msg.args[arg_pos++] = ARG_STAT;
    call_msg_store_positive_long(st->st_dev);
    call_msg_store_positive_long(st->st_ino);
    call_msg_store_positive_long(st->st_mode);
    call_msg_store_positive_long(st->st_size);
    call_msg_store_positive_long(st->st_uid);
    call_msg_store_positive_long(st->st_gid);
    call_msg_store_positive_long(st->st_mtime);
  }
}

static void call_msg_direntp(struct dirent *d)
{
  if(d == NULL){
    call_msg.args[arg_pos++] = ARG_DIRENT_NULL;
  } else {
    call_msg.args[arg_pos++] = ARG_DIRENT;
    call_msg_store_positive_long(d->d_ino);
    call_msg_string0(d->d_name);
  }
}

static void call_msg_getcwd()
{
  char cwd[2000];
  getcwd(cwd, 2000);
  call_msg_string0(cwd);
}


static void suspend_on_recv(int mq, int fun_id)
{
  int res;
  fprintf(stderr, "suspend_on_recv...\n"); fflush(stderr);
  res = msgrcv(mq, &suspend_msg, sizeof(suspend_msg), pid, 0);
  fprintf(stderr, "suspend_on_recv ok\n"); fflush(stderr);
}


static void init(void){
  if(function_bitmap == NULL){
    char *ocp_watch_mq = NULL;
    init_functions();

    ocp_watch_mq = libc_getenv("OCP_WATCH_MQ_P2W");
    if( ocp_watch_mq != NULL ){
      char *endptr;
      mq_P2W = strtol(ocp_watch_mq, &endptr, 10);
      if( endptr == ocp_watch_mq ) mq_P2W = -1;
    }
    ocp_watch_mq = libc_getenv("OCP_WATCH_MQ_W2P");
    if( ocp_watch_mq != NULL ){
      char *endptr;
      mq_W2P = strtol(ocp_watch_mq, &endptr, 10);
      if( endptr == ocp_watch_mq ) mq_W2P = -1;
    }
    if( mq_P2W == -1 || mq_W2P == -1 ){
      exit(55);
    }

    pid = getpid();
    ppid = getppid();

    if( mq_P2W != -1 ){
      struct bitmap_msg bitmap_msg;
      int res;

      call_msg_init();
      call_msg_getcwd();
      send_call(mq_P2W, PROTOCOL_P2W_ProcMsg, ppid);

      res = msgrcv(mq_W2P, &bitmap_msg, sizeof(bitmap_msg), pid, 0);
      if ( res < 0 ){
        perror("msgrcv");
        exit(2);
      }

      option_flags = bitmap_msg.flags0 + (bitmap_msg.flags1 << 8);
      nfunctions = bitmap_msg.nfuns0 + (bitmap_msg.nfuns1 << 8);
      if ( res < 20 || (res - 22) != nfunctions ){
        fprintf(stderr, "Function_bitmap: bad size %d/%d\n", res, nfunctions);
        exit(2);
      };
      for(res = 0; res < 16; res++){
        if( function_digest[res] != bitmap_msg.digest[res] ){
          fprintf(stderr, "Function bitmap: inconsistent bitmaps at pos %d\n", res);
          exit(2);
        }
      }

      function_bitmap = (char*)malloc(nfunctions);
      memcpy(function_bitmap, bitmap_msg.bitmap, nfunctions);
    }
  }
}

void _init(void){ init(); }
