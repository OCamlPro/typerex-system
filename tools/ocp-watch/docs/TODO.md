# TODO list

## Small features

* Remove ocp-watch.data file
* Canonicalize filenames, to remove .. and . in paths
* Store PATH when execvp is called
* Check for directories when open() is used (find, for example, uses
   open for directories)

## Big features

* Compute hash sums of files used as inputs
   * In the interceptor (slow)
   * In the watcher (faster, since caching is possible)
* Call commands lazily to replay a build
