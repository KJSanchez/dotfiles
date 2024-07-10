function :e() {
	vim ~/codez/dotfiles/envvars
    echo "sourcing envvars..."
	. ~/codez/dotfiles/envvars
    echo "done."
}

function delete-old-branches() {
    git branch --merged origin/master | grep -v 'master$' | xargs git branch -D
}
