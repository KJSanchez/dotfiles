function :e() {
	vim "$HOME/codez/dotfiles/local.zsh"
    echo "sourcing envvars..."
	. "$HOME/codez/dotfiles/local.zsh"
    echo "done."
    cd $DEFAULT_DIR
}

function delete-old-branches() {
    git branch --merged origin/master | grep -v 'master$' | xargs git branch -D
}
