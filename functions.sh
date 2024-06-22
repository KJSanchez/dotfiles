function :e() {
	vim ~/dotfiles/functions.sh
	. ~/dotfiles/functions.sh
}

function delete-old-branches() {
    git branch --merged origin/master | grep -v 'master$' | xargs git branch -D
}
