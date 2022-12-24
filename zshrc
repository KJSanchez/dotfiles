. ~/dotfiles/envvars
. ~/dotfiles/functions.sh

alias ls='ls -GH'
alias j='cd ..'
alias jj='cd ../..'
alias grep='grep --color=auto'
alias gti='git'
alias ibrew='arch -arm64 brew'
alias vim='nvim'
cd ~/codez/gainfulWeb

# # TODO: get zsh-completions setup
# if type brew &>/dev/null; then
#     FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
#     autoload -Uz compinit
#     compinit
# fi

eval "$(pyenv init --path --no-rehash)"

# [[ -r $(brew --prefix)/etc/profile.d/bash_completion.sh ]] && . $(brew --prefix)/etc/profile.d/bash_completion.sh
# [[ -f `brew --prefix`/etc/bash_completion.d/git-completion.bash ]] && . `brew --prefix`/etc/bash_completion.d/git-completion.bash

# echo 'eval "$(/usr/local/bin/brew shellenv)"

PROMPT='%~ λ '

export PATH="$PATH:/Users/keenan/gainfulWeb/reactjs/node_modules/.bin:/Users/keenan/gainfulWeb/nextjs/node_modules/.bin:/opt/homebrew/bin/"

QUIET=1

eval "$(starship init zsh)"

eval "$(direnv hook zsh)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
