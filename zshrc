# export XDG_CONFIG_HOME=~/dotfiles
# Maybe have a xdg_config_home directory instead?
export PYTHONDONTWRITEBYTECODE=1
export RIPGREP_CONFIG_PATH=~/.ripgreprc
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export HOMEBREW_AUTO_UPDATE_SECS=86400

alias ls='ls -GH'
alias j='cd ..'
alias jj='cd ../..'
alias grep='grep --color=auto'
alias gti='git'
alias ibrew='arch -arm64 /opt/homebrew/bin/brew'
# alias vim='nvim'

source ~/codez/dotfiles/functions.sh
source ~/codez/dotfiles/local.zsh

cd $DEFAULT_DIR

# TODO: get zsh-completions setup
if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
    autoload -Uz compinit
    compinit
fi

eval "$(pyenv init --path --no-rehash)"

[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh
[[ -r $(brew --prefix)/etc/profile.d/bash_completion.sh ]] && source $(brew --prefix)/etc/profile.d/bash_completion.sh
[[ -f $(brew --prefix)/etc/bash_completion.d/git-completion.zsh ]] && source $(brew --prefix)/etc/bash_completion.d/git-completion.zsh

eval "$(starship init zsh)"
eval "$(direnv hook zsh)"
# Disabling this to see if it breaks linting in `json/ts`
# eval "$(nodenv init -)"
# eval "$(rbenv init - zsh)"
autoload -U +X bashcompinit && bashcompinit

# >>> Rancher Desktop >>>
# Added by Rancher Desktop for nerdctl and Kubernetes
export PATH="$HOME/.rd/bin:$PATH"
if [ -f "$HOME/.rd/env" ]; then
    source "$HOME/.rd/env"
fi
# <<< Rancher Desktop <<<
