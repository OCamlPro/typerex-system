ocp-publish
===========

A tool to publish one private GIT tree to a set of public GIT
repositories.

    ocp-publish [OPTIONS] [PACKAGES]

where available OPTIONS are:
* -m STRING : a message for the commit message
* -t VERSION : tag with a version afterwards
* -l : list all packages, and exit
* -a : push all packages

ocp-publish should be run in a GIT directory. It will look for the
root of the GIT directory (.git/ directory), and from there, scan all
the subdirectories for two kinds of files:
* file ".pubconfig": contains the description of an independant project
  with lines like:

      name: ocp-build
      git:  git@github.com:OCamlPro/ocp-build 

* file ".pubignore": contains a list of files and directory to ignore
    when scanning

For each package listed in PAKCAGES (or all, with -a), ocp-publish
will checkout the corresponding repository in .pub/ (at the root of
the GIT directory), copy the files in that directory, git-add and
git-commit them, before git-pushing them to the public repositories
(after trying on a pre-publish branch that will be deleted
afterwards).

