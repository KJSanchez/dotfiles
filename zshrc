. ~/codez/dotfiles/envvars
. ~/codez/dotfiles/functions.sh

alias ls='ls -GH'
alias j='cd ..'
alias jj='cd ../..'
alias grep='grep --color=auto'
alias gti='git'
alias ibrew='arch -arm64 brew'
# alias vim='nvim'

cd $DEFAULT_DIR

# # TODO: get zsh-completions setup
# if type brew &>/dev/null; then
#     FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
#     autoload -Uz compinit
#     compinit
# fi

eval "$(pyenv init --path --no-rehash)"

# # >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/usr/local/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/usr/local/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/usr/local/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/usr/local/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup


function conda_audiofocus_activate() {
    if [[ $PWD == /Users/keenansanchez/codez/audiofocus* ]] ; then
        conda activate audiofocus3.7
    else
        conda deactivate
    fi
}
# conda activate audiofocus3.7
chpwd_functions+=(conda_audiofocus_activate)

# <<< conda initialize <<<


# [[ -r $(brew --prefix)/etc/profile.d/bash_completion.sh ]] && . $(brew --prefix)/etc/profile.d/bash_completion.sh
# [[ -f `brew --prefix`/etc/bash_completion.d/git-completion.bash ]] && . `brew --prefix`/etc/bash_completion.d/git-completion.bash

# echo 'eval "$(/usr/local/bin/brew shellenv)"

export PATH="$PATH:/opt/homebrew/bin"
export PATH="$PATH:$HOME/.cargo/bin"

eval "$(starship init zsh)"

# eval "$(direnv hook zsh)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

 # eval "$(nodenv init -)"

# eval "$(rbenv init - zsh)"
