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

function enum-headers() {
    local input="${1:-/dev/stdin}"
    head -1 "$input" | tr , '\n' | nl
}

function table() {
    column -s, -t "$1"
}

function what-changed() {
    TAG1=2025-04-15-prd-03
    TAG2=2025-04-23-stg-01
    REPO="K-Hovnanian-Digital-Products/dp-warranty-frontend"

    # Get commit dates
    DATE1=$(gh api repos/$REPO/git/ref/tags/$TAG1 --jq '.object.sha' | xargs -I{} gh api repos/$REPO/commits/{} --jq '.commit.committer.date')
    DATE2=$(gh api repos/$REPO/git/ref/tags/$TAG2 --jq '.object.sha' | xargs -I{} gh api repos/$REPO/commits/{} --jq '.commit.committer.date')

    # Fetch PRs merged in that date range
    gh pr list --repo $REPO --state merged --json number,title,mergedAt,author,url --jq ".[] | select(.mergedAt >= \"$DATE1\" and .mergedAt <= \"$DATE2\")"
    # gh pr list --repo $REPO --state merged --json number,title,mergedAt,author,url --jq ".[] | select(.mergedAt >= \"$DATE1\" and .mergedAt <= \"$DATE2\" and .author.login == \"salman5436\")"
}


function get-pr-summaries() {
    gh pr list --state merged --author @me --json url --limit 10000
}
