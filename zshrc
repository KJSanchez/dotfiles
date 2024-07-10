alias ls='ls -GH'
alias j='cd ..'
alias jj='cd ../..'
alias grep='grep --color=auto'
alias gti='git'
alias ibrew='arch -arm64 brew'
# alias vim='nvim'

. ~/codez/dotfiles/functions.sh
. ~/codez/dotfiles/envvars

cd $DEFAULT_DIR

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# # TODO: get zsh-completions setup
# if type brew &>/dev/null; then
#     FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
#     autoload -Uz compinit
#     compinit
# fi

# eval "$(pyenv init --path --no-rehash)"

__conda_setup="$('/usr/local/Caskroom/miniconda/base/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/usr/local/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup

function conda_activate() {
    if [[ $PWD == /Users/$USER/codez/audiofocus* ]] ; then
        conda activate audiofocus3.7
    elif [[ $PWD == /Users/$USER/codez/cloud-frontend ]]; then
        conda activate frontend
    elif [[ $PWD == /Users/$USER/codez/cloud-backend ]]; then
        conda activate cloud-backend
    else
        conda deactivate
    fi
}
chpwd_functions+=(conda_activate)
conda_activate

# <<< conda initialize <<<


# [[ -r $(brew --prefix)/etc/profile.d/bash_completion.sh ]] && . $(brew --prefix)/etc/profile.d/bash_completion.sh
# [[ -f `brew --prefix`/etc/bash_completion.d/git-completion.bash ]] && . `brew --prefix`/etc/bash_completion.d/git-completion.bash

# echo 'eval "$(/usr/local/bin/brew shellenv)"


eval "$(starship init zsh)"

# eval "$(direnv hook zsh)"
 # eval "$(nodenv init -)"
# eval "$(rbenv init - zsh)"



autoload -U +X bashcompinit && bashcompinit

# complete -o nospace -C /usr/local/bin/terraform terraform
