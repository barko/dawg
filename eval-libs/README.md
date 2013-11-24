This directory contains support libraries in various languages for
evaluating EigenDog models.  The `dawg gen` subcommand generates
self-contained source code to evaluate models, with minimal
dependencies on external libraries.  In contrast, the libraries in
this directory interpret the model files and construct the necessary
abstractions in memory to evaluate them efficiently.  This alternative
makes managing multiple models easier.

