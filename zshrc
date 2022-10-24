. ~/dotfiles/envvars
. ~/dotfiles/functions.sh

alias ls='ls -GH'
alias j='cd ..'
alias jj='cd ../..'
alias grep='grep --color=auto'
alias gti='git'
alias ibrew='arch -arm64 brew'
cd ~/gainfulWeb

# # TODO: get zsh-completions setup
# if type brew &>/dev/null; then
#     FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
#     autoload -Uz compinit
#     compinit
# fi

eval "$(pyenv init --path)"

[[ -r $(brew --prefix)/etc/profile.d/bash_completion.sh ]] && . $(brew --prefix)/etc/profile.d/bash_completion.sh
# [[ -f `brew --prefix`/etc/bash_completion.d/git-completion.bash ]] && . `brew --prefix`/etc/bash_completion.d/git-completion.bash
#

# echo 'eval "$(/usr/local/bin/brew shellenv)"

PROMPT='%~ %# '
