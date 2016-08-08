/***********************************************************************/
/*                                                                     */
/*                             ocp-watch                               */
/*                                                                     */
/*  Copyright 2012 OCamlPro SAS                                        */
/*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    */
/*                                                                     */
/***********************************************************************/

#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/unixsupport.h"

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <errno.h>

int msgget(key_t key, int msgflg);

value msgqueue_private_ml(value unit)
{
  int fd = msgget(IPC_PRIVATE, 0600);
  if( fd == -1 ) uerror("msgget", Nothing);
  return Val_int(fd);
}

value msgqueue_receive_ml(value fd_v, value askid_v, value msg_v)
{
  value id_v = Field(msg_v, 0);
  /*  value size_v = Field(msg_v, 1); */
  value buf_v = Field(msg_v, 2);
  long id = Long_val(id_v);
  long askid = Long_val(askid_v);

  long *ids = (long*) buf_v;
  int buflen = string_length(buf_v);
  int fd = Int_val(fd_v);
  long prev_value = ids[-1];
  long id_recv;

  while(1){
    ids[-1] = id;
    int res = msgrcv(fd, &ids[-1], buflen + sizeof(long), askid, MSG_NOERROR);
    id_recv = ids[-1];
    ids[-1] = prev_value;
    if( res < 0 ) {
      if( errno != EINTR) uerror("msgrcv", Nothing);
    } else {
      Field(msg_v, 0) = Val_int(id_recv);
      Field(msg_v, 1) = Val_int(res);
      return Val_unit;
    }
  }
}

value msgqueue_send_ml(value fd_v, value msg_v)
{
  value id_v = Field(msg_v, 0);
  value size_v = Field(msg_v, 1);
  value buf_v = Field(msg_v, 2);

  /*  char *buf = String_val(buf_v); */
  long *ids = (long*) buf_v;
  int buflen = string_length(buf_v);
  int fd = Int_val(fd_v);
  int len = Int_val(size_v);
  long id = Long_val(id_v);
  long prev_value = ids[-1];

  if( len > buflen ) invalid_argument("Msgipc.send: len > String.length(buf)");

  while(1){
    ids[-1] = id;
    int res = msgsnd(fd, &ids[-1], len, MSG_NOERROR);
    ids[-1] = prev_value;

    if( res < 0 ) {
      if( errno != EINTR) uerror("msgsnd", Nothing);
    } else {
      return Val_unit;
    }
  }
  return Val_unit;
}

value msgqueue_delete_ml(value fd_v)
{
  int fd = Int_val(fd_v);

  if ( msgctl(fd, IPC_RMID, NULL) < 0 ) uerror("msgctl", Nothing);
  return Val_unit;
}


value msgqueue_sizeofid_ml(value unit)
{
  return Val_int(sizeof(long));
}
