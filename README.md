Git-prompt 
==========

git-prompt is a shell-independent utility that renders information from a local git repository.

Build
-----
To build this package run the following commands:

* cabal install --only-dep

* runhaskell Setup configure --user
* runhaskell Setup build
* runhaskell Setup install


This will install the binary in ~/.cabal/bin/ directory.

Bash
----

To use it with bash, configure the shell prompt as follow:

`PS1='\u@\h :: \[\033[1;32m\]\w\[\033[0m\] $(~/.cabal/bin/git-prompt cyan)\n-> '`


