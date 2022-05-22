function :e() {
	vim ~/.zshrc
	. ~/.zshrc
}

alias ls='ls -GH'
alias j='cd ..'
alias jj='cd ../..'
alias grep='grep --color=auto'
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

export PATH="$HOME/.pyenv/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
export LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
export CPPFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"
