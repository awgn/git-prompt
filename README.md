Git-prompt 
==========

git-prompt is a shell-independent utility that renders information from a local git repository.
It is written in Haskell.

Build
-----
To build this package run the following commands:

```
cabal install --only-dep
runhaskell Setup configure --user
runhaskell Setup build
runhaskell Setup install
```

This will install the binary `git-prompt` in ~/.cabal/bin/ directory.

Bash
----

To use it with bash, configure the shell prompt as follow:

`PS1='\u@\h :: \[\033[1;32m\]\w\[\033[0m\] $(~/.cabal/bin/git-prompt cyan)\n-> '`

Zsh
---

To use it with zsh, here's a simple configuration of .zshrc:

```
autoload -U colors && colors
setopt promptsubst
local git_prompt='$(/root/.cabal/bin/git-prompt blue)'
PS1="%{$fg[green]%}%n@%m %{$fg[blue]%}%c ${git_prompt} %# "
```


