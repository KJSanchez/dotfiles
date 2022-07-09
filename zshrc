. ~/dotfiles/envvars
. ~/dotfiles/functions.sh

alias ls='ls -GH'
alias j='cd ..'
alias jj='cd ../..'
alias grep='grep --color=auto'
alias gti='git'
cd ~/gainfulWeb

# # TODO: get zsh-completions setup
# if type brew &>/dev/null; then
#     FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
#     autoload -Uz compinit
#     compinit
# fi

# TODO: why do I need this??
eval $(/opt/homebrew/bin/brew shellenv)
eval "$(pyenv init --path)"
