# aws-lambda-cobol

## Building

This build could probably use some cleanup.  As it stands you need to build the lambda first because some shared libraries (libcob) are produced from the lambda build image that need to be included with the layer.

If you want to make everything simply do
```
make all
```
For other tasks, see the documentation in `Makefile`.

## Things that made this challenging
1. gnucobol compiler is a challenging abstraction on top of GCC, which is complex to build with
2. dependency management in C is completely alien to me, so had to try to apply some of my linux user knowledge to get some things working
3. documentation about using layers in combination with custom runtimes is sparse and often skips over pieces needed to understand how they work together.  this is often times because many people do this with interpreted languages, which are markedly easier to implement
4. If you don't want to incur the performance penalty of running an image on lambda, it becomes a pain to manage dependencies because the runtime has no package manager
5. since we're building what is essentially a C application that needs to run on lambda, it has to be built on a binary compatible distribution with the correct version of GCC & all that, which forces me to use unfamiliar tooling
6. There are no good IDEs for this--the ones that do exist are difficult to configure


## Things I learned
1. C compilers are complicated
2. Building with docker can be interesting
    * Issue deploying old code because of caching with COPY vs VOLUME
3. GNU COBOL is surprisingly well documented, but there is a lot of implied knowledge about C/GCC
4. Making a custom runtime isn't really that complicated, and neither are layers for modern languages
5. Layers can be a great way to side-load common dependencies to reduce deployable size
6. Layers are somewhat limited - 5 layers/lambda & <250mb in size
