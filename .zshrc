# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e

autoload -U select-word-style
select-word-style bash

zstyle :compinstall filename '/home/java/.zshrc'

export PYTHONDOCS=/usr/share/doc/python/html/

PROMPT=$'%{\e[1;32m%}%T %{\e[0m%}%m %{\e[0;36m%}[%{\e[1;36m%}%c%{\e[0;36m%}]%{\e[1;35m%}%#%{\e[0m%} '
alias py=ipython
alias ls='ls --color'
alias xvnc='x11vnc -notruecolor -forever -display :0 -usepw'
alias cp='cp -v'
alias grep='grep --color=auto'
autoload -Uz compinit
compinit

export EDITOR="emacs -nw"
