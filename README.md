Git-prompt 
==========

GitPrompt is an utility that, from a local git repository, renders information 
into a string suitable to be embedded in the shell prompt. It is written in Haskell.

Build
-----
To build the package run the following commands:

```
cabal install --only-dep
runhaskell Setup configure --user
runhaskell Setup build
runhaskell Setup install
```

or 

```
stack build
stack install
```

This will build and install the binary `GitPrompt` in either ~/.cabal/bin/ or
~/.local/bin/ directory.

Usage
-----

```
GitPrompt!

Usage: GitPrompt [-p|--path PATH] [-t|--theme COLOR] [-s|--short] [-V|--version]

Available options:
  -h,--help                Show this help text
  -p,--path PATH           Specify the git-rository path ($PWD by default)
  -t,--theme COLOR         Specify the color theme
  -s,--short               Use short mode
  -V,--version             Print version
```


Bash
----

To use it with bash, configure the shell prompt as follow:

`PS1='\u@\h :: \[\033[1;32m\]\w\[\033[0m\] $(~/.local/bin/GitPrompt --theme blue)\n-> '`

Zsh
---

For zsh, try the following configuration in .zshrc:

```
autoload -U colors && colors
setopt promptsubst
local git_prompt='$(/root/.local/bin/GitPrompt --theme blue)'
PS1="%{$fg[green]%}%n@%m %{$fg[blue]%}%c ${git_prompt} %# "
```

Fish
----

For fish shell, define the following function in 
~/.config/fish/function/fish\_prompt.fish:


```
function fish_prompt --description 'Write out the prompt'
	
	set -l last_status $status
    set -l git_prompt (~/.local/bin/GitPrompt --theme blue)

	if not set -q __fish_prompt_normal
		set -g __fish_prompt_normal (set_color normal)
	end

	# PWD
	set_color $fish_color_cwd
	echo -n (prompt_pwd)
	set_color normal

	printf '%s '{"$git_prompt"} 

	if not test $last_status -eq 0
	set_color $fish_color_error
	end

	echo -n '$ '

end
```
