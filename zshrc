function :e() {
	vim ~/.zshrc
	. ~/.zshrc
}

alias ls='ls -GH'
alias j='cd ..'
alias jj='cd ../..'
alias grep='grep --color=auto'
alias gti='git'
cd ~/gainfulWeb
export XDG_CONFIG_HOME=~/dotfiles

# # TODO: get zsh-completions setup
# if type brew &>/dev/null; then
#     FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
#     autoload -Uz compinit
#     compinit
# fi

# TODO: why do I need this??
eval $(/opt/homebrew/bin/brew shellenv)

# export PATH="$HOME/.pyenv/bin:$PATH"
# export PATH="/usr/local/bin:$PATH"
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"

export PATH="$HOME/.ebcli-virtual-env/executables:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"

# Do not create .pyc files or __pycahce__
export PYTHONDONTWRITEBYTECODE=1

export RIPGREP_CONFIG_PATH=~/.ripgreprc

function venv() {
    . venv/bin/activate
}

function gtest() {
    ~/gainfulWeb/venv/bin/python manage.py test -k -n -v3 --failfast $1
}
