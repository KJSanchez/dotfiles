function :e() {
	vim "$HOME/codez/dotfiles/local.zsh"
    echo "sourcing envvars..."
	. "$HOME/codez/dotfiles/local.zsh"
    echo "done."
    cd $DEFAULT_DIR
}

function delete-old-branches() {
    git branch --merged origin/main | grep -v 'main$' | xargs git branch -D
}

function summarize-khov-resume() {
    for project in dp-warranty-frontend dp-warranty-backend dp-infra; do
        gh pr ls --author @me --json title,body,id --state merged --limit 10000 --repo https://github.com/K-Hovnanian-Digital-Products/$project > /tmp/$project.json
    done
}
