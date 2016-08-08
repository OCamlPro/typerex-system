
type entry = {
  ctime_sec : int64;
  ctime_nanosec : int64;
  mtime_sec : int64;
  mtime_nanosec : int64;
  dev : int64;
  ino : int64;
  mode : int64;
  uid : int64;
  gid : int64;
  file_size : int64;
  name : string;
}

type index = {
  version : int;
  entries : entry array;
}

val of_file : string -> index


