# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e

export PATH="/usr/local/git/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:$PATH"

# Rbenv
eval "$(rbenv init -)"

autoload -U select-word-style
select-word-style bash

zstyle :compinstall filename '/home/java/.zshrc'

export PYTHONDOCS=/usr/share/doc/python/html/

PROMPT=$'%{\e[1;32m%}%T %{\e[0m%}%n %{\e[0;36m%}[%{\e[1;36m%}%c%{\e[0;36m%}]%{\e[1;35m%}%#%{\e[0m%} '


alias ls='ls -Gp'
alias cp='cp -v'
alias nano='emacsclient -c "$@"'
alias grep='grep --color=auto'

autoload -Uz compinit
compinit

export EDITOR='emacs -nw'
