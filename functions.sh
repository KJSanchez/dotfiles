function :e() {
	vim ~/codez/dotfiles/locals.zsh
    echo "sourcing envvars..."
	. ~/codez/dotfiles/locals.zsh
    echo "done."
}

function delete-old-branches() {
    git branch --merged origin/master | grep -v 'master$' | xargs git branch -D
}
