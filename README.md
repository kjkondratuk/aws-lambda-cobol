# cobol-docker

A docker container for compiling and executing GNU COBOL applications.  It contains a GNU COBOL installation with cJSON and libxml2 for JSON and XML parsing functionality.

## Mounts

In order to make source available, code can be mounted to `/root` using volumes.  The volume can be created locally to serve up the current directory by issuing `make volumes` in the project directory.

## TODO

1. Look to add https://cobolget.com/ 