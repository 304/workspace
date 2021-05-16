# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e

export PATH="/usr/local/git/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:$PATH"

autoload -U select-word-style
select-word-style bash

export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode

PROMPT=$'%{\e[1;32m%}%T %{\e[0m%}%n %{\e[0;36m%}[%{\e[1;36m%}%c%{\e[0;36m%}]%{\e[1;35m%}%#%{\e[0m%} '


eval "$(zoxide init zsh)"

alias cat='bat'
alias ls='ls -Gp'
alias cp='cp -v'
alias emacs='emacsclient -c "$@"'
alias grep='grep --color=auto'

autoload -Uz compinit
compinit
