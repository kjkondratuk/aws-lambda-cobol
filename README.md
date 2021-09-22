# cobol-docker

A docker container for compiling and executing GNU COBOL applications.  It contains a GNU COBOL installation with cJSON and libxml2 for JSON and XML parsing functionality.  It is similar to https://github.com/OlegKunitsyn/gnucobol-docker but it uses local installations of GNU COBOL when building the container.  To that end, it could be used to produce an image for any version of the compiler.

## Mounts

In order to make source available, code can be mounted to `/root` using volumes.  The volume can be created locally to serve up the current directory by issuing `make volumes` in the project directory.

