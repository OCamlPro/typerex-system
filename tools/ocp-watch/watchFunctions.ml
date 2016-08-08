(*
 for "tar", we need:
-  openat(), mkdirat(), newfstatat(), unlinkat(),utimensat()
-  AT_FDCWD : at current directory
-  clone()
*)


(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

type call_flags =
  | KILL_BEFORE (* don't execute, just kill program *)
  | CLOSE_BEFORE (* close socket before *)
  | NO_IMPLEMENTATION
  | NO_RETURN
  | REPLACE_CALL_BY of string
  | CHECK_FOR_OUTPUT of string
  | UPDATE_PID
  | INT_OF_STRING_RESULT
  | INPUT_FILE
  | SEND_CWD
  | SEND_AS of string

type arg_flags =
    SEND_INPUT  (* try to send this argument as input to call *)
  | SEND_OUTPUT (* try to send this argument only after call *)
  | MAYBE_ARG
  | VA_ARRAY

(* declarations *)

let functions = [|

  "int", "dup", [ "int", [ SEND_INPUT ] ], [];
  "int", "dup2", [
    "int", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
  ], [];
  "int", "dup3", [
    "int", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
  ], [];

  "int", "fcntl", [
    "int", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "int", [ MAYBE_ARG ];
  ], [];

  "int", "execl", [
    "const char *", [ SEND_INPUT ];
    "const char *", [ VA_ARRAY; SEND_INPUT ];
  ], [ REPLACE_CALL_BY "libc_execv" ];

  "int", "execlp", [
    "const char *", [ SEND_INPUT ];
    "const char *", [ VA_ARRAY; SEND_INPUT ];
  ], [ REPLACE_CALL_BY "libc_execvp" ];

  "int", "execle", [
    "const char *", [ SEND_INPUT ];
    "const char *", [ VA_ARRAY; SEND_INPUT ];
    "char * const *", [ (* SEND_INPUT *) ]
  ], [ REPLACE_CALL_BY "libc_execvpe" ];

  "int", "execv", [
    "const char *", [ SEND_INPUT ];
    "char * const *", [ SEND_INPUT ]
  ], [ CLOSE_BEFORE ];

  "int", "execvp", [
    "const char *", [ SEND_INPUT ];
    "char * const *", [ SEND_INPUT ]
  ], [ CLOSE_BEFORE ];

  "int", "execvpe", [
    "const char *", [ SEND_INPUT ];
    "char * const *", [ SEND_INPUT ];
    "char * const *", [ (* SEND_INPUT *) ]
  ], [ CLOSE_BEFORE ];

  "time_t", "time", [
    "time_t *", []
  ], [] ;

  "int", "chmod", [
    "const char *", [ SEND_INPUT ];
    "mode_t", [ SEND_INPUT ]
  ], [] ;

  "int", "chown", [
    "const char *", [ SEND_INPUT ];
    "uid_t", [ SEND_INPUT ];
    "gid_t", [ SEND_INPUT ]
  ], [] ;

  "int", "chroot", [
    "const char *", [ SEND_INPUT ]
  ], [] ;

  "int", "fchmod", [
    "int", [ SEND_INPUT ];
    "mode_t", [ SEND_INPUT ]
  ], [] ;

  "int", "fchown", [
    "int", [ SEND_INPUT ];
    "uid_t", [ SEND_INPUT ];
    "gid_t", [ SEND_INPUT ]
  ], [] ;

  "int", "access", [
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ]
  ], [];
  "int", "stat", [
    "const char *", [ SEND_INPUT ];
    "struct stat *", [ SEND_OUTPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];
  "int", "fstat", [
    "int", [ SEND_INPUT ];
    "struct stat *", [ SEND_OUTPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];
  "int", "lstat", [
    "const char *", [ SEND_INPUT ];
    "struct stat *", [ SEND_OUTPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];


  "int", "fgetc", [
    "FILE *", [ SEND_INPUT ]
  ], [];

  "char *", "fgets", [
    "char *", [];
    "int", [];
    "FILE *", [ SEND_INPUT ]
  ], [ INT_OF_STRING_RESULT ];

  "int", "getc", [
    "FILE *", [ SEND_INPUT ]
  ], [];

  "int", "getchar", [ ], [];

  "char *", "gets", [
    "char *", []
  ], [ INT_OF_STRING_RESULT ];

  "int", "ungetc", [
    "int", [];
    "FILE *", [ SEND_INPUT ]
  ], [];

  "DIR *", "opendir", [
    "const char *", [ SEND_INPUT ]
  ], [];

  "DIR *", "fdopendir", [
    "int", [ SEND_INPUT ]
  ], [];

  "int", "closedir", [
    "DIR *", [ SEND_INPUT ]
  ], [];

  "struct dirent *", "readdir", [
    "DIR *", [ SEND_INPUT ]
  ], [];

  "int", "fputc", [
    "int", [];
    "FILE *", [ SEND_INPUT ]
  ], [];
  "int", "fputs", [
    "const char *", [];
    "FILE *", [ SEND_INPUT ]
  ], [];
  "int",  "putc", [
    "int", [];
    "FILE *", [ SEND_INPUT ]
  ], [];
  "int", "putchar", [
    "int", []
  ], [];
  "int", "puts", [
    "const char *", []
  ], [];

  "ssize_t", "read", [
    "int", [ SEND_INPUT ];
    "void *", [];
    "size_t", []
  ], [];
  "size_t", "fread", [
    "void *", [];
    "size_t", [ SEND_INPUT ];
    "size_t", [];
    "FILE *", [ SEND_INPUT ]
  ], [];
  "ssize_t", "write", [
    "int", [ SEND_INPUT ];
    "const void *", [];
    "size_t", [ SEND_INPUT ]
  ], [];
  "size_t", "fwrite", [
    "const void *", [];
    "size_t", [ SEND_INPUT ];
    "size_t", [ SEND_INPUT ];
    "FILE *", [ SEND_INPUT ]
  ], [];

  "FILE *", "fopen", [
    "const char *", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ]
  ], [] ;
  "int", "open", [
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "mode_t", [ MAYBE_ARG; SEND_INPUT ]
  ], [] ;
  "int", "__open", [
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "mode_t", [ MAYBE_ARG; SEND_INPUT ]
  ], [] ;
  "int", "__open_2", [
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "mode_t", [ MAYBE_ARG; SEND_INPUT ]
  ], [] ;
  "int", "creat", [
    "const char *", [ SEND_INPUT ];
    "mode_t", [ SEND_INPUT ]], [] ;



  "int", "close", [
    "int", [ SEND_INPUT ]
  ], [];
  "int", "fclose", [
    "FILE *", [ SEND_INPUT ]
  ], [];

  "int", "truncate", [
    "const char *", [ SEND_INPUT ];
    "off_t", [ SEND_INPUT ]
  ], [] ;
  "int", "ftruncate", [
    "int", [ SEND_INPUT ];
    "off_t", [ SEND_INPUT ]
  ], [] ;

  "int", "lchown", [
    "const char *", [ SEND_INPUT ];
    "uid_t", [ SEND_INPUT ];
    "gid_t", [ SEND_INPUT ]
  ], [] ;
  "int", "mkdir", [
    "const char *", [ SEND_INPUT ];
    "mode_t", [ SEND_INPUT ]
  ], [] ;
  "int", "rmdir", [
    "const char *", [ SEND_INPUT ]
  ], [] ;

  "int", "unlink", [
    "const char *", [ SEND_INPUT ]
  ], [] ;
  "int", "remove", [ (* = unlink or rmdir *)
    "const char *", [ SEND_INPUT ]
  ], [] ;
  "int", "link", [
    "const char *", [ SEND_INPUT ]; (* oldpath *)
    "const char *", [ SEND_INPUT ]; (* newpath *)
  ], [];
  "int", "rename", [
    "const char *", [ SEND_INPUT ]; (* oldpath *)
    "const char *", [ SEND_INPUT ]; (* newpath *)
  ], [];

  "char *", "getenv", [
    "const char *", [ SEND_INPUT ]
  ], [] ;
  "int", "putenv", [
    "char *", [ SEND_INPUT ]
  ], [] ;
  "int", "execve", [
    "const char *", [ SEND_INPUT ];
    "char * const *", [ SEND_INPUT ];
    "char * const *", [ (* SEND_INPUT *) ]
  ], [ CLOSE_BEFORE ] ;


  "int", "creat64", [
    "const char *", [ SEND_INPUT ];
    "mode_t", [ SEND_INPUT ]
  ], [] ;
  "FILE *", "fopen64", [
    "const char *", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ]], [] ;
  "int", "ftruncate64", [
    "int", [ SEND_INPUT ];
    "__off64_t", [ SEND_INPUT ]
  ], [] ;

  "int", "open64", [
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "mode_t", [ MAYBE_ARG; SEND_INPUT ]
  ], [] ;
  "int", "__open64", [
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "mode_t", [ MAYBE_ARG; SEND_INPUT ]
  ], [] ;
  "int", "__open64_2", [
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "mode_t", [ MAYBE_ARG; SEND_INPUT ]
  ], [] ;



  "pid_t", "fork", [], [ UPDATE_PID ];
  "pid_t", "vfork", [], [ REPLACE_CALL_BY "libc_fork"; UPDATE_PID ];
  "int",  "system", [ "const char *", [SEND_INPUT] ], [];

  "int", "__fxstat", [
    "int", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "struct stat *", [ SEND_OUTPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];
  "int", "__xstat", [
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
    "struct stat *", [ SEND_OUTPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];
  "int", "__lxstat", [
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
    "struct stat *", [ SEND_OUTPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];


  "int", "__fxstat64", [
    "int", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "struct stat64 *", [ SEND_OUTPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];

  "int", "__xstat64", [
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
    "struct stat64 *", [ SEND_OUTPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];

  "int", "__lxstat64", [
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
    "struct stat64 *", [ SEND_OUTPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];

  "int", "__fxstatat64", [
    "int", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
    "struct stat64 *", [ SEND_OUTPUT ];
    "int", [ SEND_INPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];

  "void", "exit", [
    "int", [ SEND_INPUT ]
  ], [ CLOSE_BEFORE; NO_RETURN ];

  "int", "chdir", [
    "const char *", [ SEND_INPUT ];
  ], [];

  "int", "fchdir", [
    "int", [ SEND_INPUT ];
  ], [ SEND_CWD ];

  "int", "utime", [
    "const char *", [ SEND_INPUT ];
    "const struct utimbuf *", [];
  ], [ CHECK_FOR_OUTPUT "res == 0" ];

  "int", "utimes", [
    "const char *", [ SEND_INPUT ];
    "const struct timeval *", [];
  ], [ CHECK_FOR_OUTPUT "res == 0" ];

  "FILE *", "fdopen", [
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
  ], [];

  "FILE *", "freopen", [
    "const char *", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
    "FILE *", [ SEND_INPUT ];
  ], [];
  "FILE *", "freopen64", [
    "const char *", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
    "FILE *", [ SEND_INPUT ];
  ], [];

  "ssize_t", "readlink", [
    "const char *", [ SEND_INPUT ];
    "char *", [ SEND_OUTPUT ];
    "size_t", [ ];
  ], [ CHECK_FOR_OUTPUT "res > 0"];

  "int", "symlink", [
    "const char *", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
  ], [];



                (* -at functions: take a directory file descriptor
                   before the filename, to anchor relative filenames
                   in that directory.  A particular case is AT_FDCWD
                   (-100) that corresponds to the current
                   directory. *)
  "int", "openat", [
    "int", [ SEND_INPUT ]; (* AT_FDCWD = -100 *)
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "mode_t", [ MAYBE_ARG; SEND_INPUT ]
  ], [] ;
  "int", "__openat_2", [
    "int", [ SEND_INPUT ]; (* AT_FDCWD = -100 *)
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "mode_t", [ MAYBE_ARG; SEND_INPUT ]
  ], [] ;
  "int", "openat64", [
    "int", [ SEND_INPUT ]; (* AT_FDCWD = -100 *)
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "mode_t", [ MAYBE_ARG; SEND_INPUT ]
  ], [] ;
  "int", "__openat64_2", [
    "int", [ SEND_INPUT ]; (* AT_FDCWD = -100 *)
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "mode_t", [ MAYBE_ARG; SEND_INPUT ]
  ], [] ;

  "int", "renameat", [
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ]; (* oldpath *)
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ]; (* newpath *)
  ], [];

  "int", "__fxstatat", [
    "int", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
    "struct stat *", [ SEND_OUTPUT ];
    "int", [ SEND_INPUT ]
  ], [ CHECK_FOR_OUTPUT "res == 0" ];

  "int", "mkdirat", [
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
    "mode_t", [ SEND_INPUT ]
  ], [] ;

  "int", "linkat", [
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ]; (* oldpath *)
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ]; (* newpath *)
    "int", [ SEND_INPUT ];
  ], [];

  "int", "unlinkat", [
    "int", [ SEND_INPUT ]; (* dirfd *)
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ]; (* flags AT_REMOVEDIR*)
  ], [] ;

  "ssize_t", "readlinkat", [
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
    "char *", [ SEND_OUTPUT ];
    "size_t", [ ];
  ], [];

  "int", "symlinkat", [
    "const char *", [ SEND_INPUT ];
    "int", [ SEND_INPUT ];
    "const char *", [ SEND_INPUT ];
  ], [];

                |]

let function_name id =
  let (_, fun_name, _, _) = functions.(id) in
  fun_name

module StringSet = Set.Make(String)
let digests = ref StringSet.empty

let digest =
  let buf = Buffer.create 1000 in
  Array.iter (fun (_, fun_name, _, _) ->
    Buffer.add_string buf fun_name;
    let d = Digest.string (Buffer.contents buf) in
    digests := StringSet.add d !digests;
  ) functions;
  let d = Digest.string (Buffer.contents buf) in
  (d : string)


