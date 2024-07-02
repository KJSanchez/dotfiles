function :e() {
	vim ~/codez/dotfiles/envvars
	. ~/codez/dotfiles/envvars
}

function delete-old-branches() {
    git branch --merged origin/master | grep -v 'master$' | xargs git branch -D
}
