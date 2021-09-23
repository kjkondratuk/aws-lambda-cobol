# cobol-docker

## Building

This build could probably use some cleanup.  As it stands you need to build the lambda first because some shared libraries (libcob) are produced from the lambda build image that need to be included with the layer.

If you want to make everything simply do
```
make all
```

### Lambda
Executes a build of the application inside of a docker container with binary compatibility for AWS Lambda.  The second command will then
```
make run
make create-lambda
```

### GNU COBOL Layer
```
make create-layer
```

## TODO
1. Figure out how to enable JSON library
2. Build/find a REST client to interact with other AWS and/or Fetch Services
3. Add AWS CLI commands to automatically publish artifacts from makefile

## Things that made this challenging
1. gnucobol compiler is a challenging abstraction on top of GCC, which is complex to build with
2. dependency management in C is completely alien to me, so had to try to apply some of my linux user knowledge to get some things working
3. documentation about using layers in combination with custom runtimes is sparse and often skips over pieces needed to understand how they work together.  this is often times because many people do this with interpreted languages, which are markedly easier to implement
4. If you don't want to incur the performance penalty of running an image on lambda, it becomes a pain to manage dependencies because the runtime has no package manager
5. since we're building what is essentially a C application that needs to run on lambda, it has to be built on a binary compatible distribution with the correct version of GCC & all that, which forces me to use unfamiliar tooling
6. There are no good IDEs for this--the ones that do exist are difficult to configure


## Things I learned
1. It doesn't seem that this COBOL library has implemented standard methods of dealing with variable-length data from the command line (PIC S9(4) COMP-4 + PIC X(N))
2. 