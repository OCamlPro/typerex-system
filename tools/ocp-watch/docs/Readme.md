# ocp-watch

A tool to observe a compilation process. It uses LD_PRELOAD to intercept calls
to the glibc, and tries to infer which commands are executed and which files
are used or modified in the process.

## Installation:

    ocp-watch install

Builds the shared library used to intercept function calls, and install it
in ~/.ocp-watch/


## Recording a build:

    ocp-watch record make all

Call `make all`, and record all the function calls in a file called
`ocp-watch.bin`.

    ocp-watch record -o ocp-watch2.bin make install

Call `make install`, and record all function calls in a file called
`ocp-watch2.bin`.

    ocp-watch record -kill-file ocp-watch.k make all

Call `make all`, and record all function calls, except the ones listed
in `ocp-watch.k` (one function name per line).

## Playing a build:

    ocp-watch report -calls ocp-watch.bin

Print all the function calls recorded in `ocp-watch.bin`

    ocp-watch report -calls -pid 38383 ocp-watch.bin

Print all the function calls recorded in `ocp-watch.bin` for process
with pid 38383.

    ocp-watch report -pstree ocp-watch.bin

Display the trace as a tree of processes

    ocp-watch report -pstree -pid 39393 ocp-watch.bin

Idem, but only for process 39393 and its sub-processes.

    ocp-watch report -pstree -open ocp-watch.bin

Display the tree of processes, and the files opened (ro,wo or rw) by the
processes.

    ocp-watch report -pstree -files ocp-watch.bin

Display the tree of processes, plus, for each process, the dependencies
on files.

