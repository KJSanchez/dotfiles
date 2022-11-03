function :e() {
	vim ~/dotfiles/functions.sh
	. ~/dotfiles/functions.sh
}

function venv() {
    . venv/bin/activate
}

function unused-endpoints() {
    awk -F '[`"]' '{sub(/[\?\$].*/,""); $2 != ""; if ($2) print $2}' ~/gainfulWeb/reactjs/src/api/endpoints.ts > /tmp/react-endpoints

	QUIET=1 DJANGO_SETTINGS_MODULE=settings.development ~/gainfulWeb/venv/bin/python ~/gainfulWeb/manage.py show_urls --format=pretty-json | jq -r 'map(select(.module | startswith("api") or startswith("blog") or startswith("consultation") or startswith("ebdjango") or startswith("influencers") or startswith("profiles") or startswith("reviews") or startswith("sms")) | .url) | sort | .[]' > /tmp/all-endpoints

    cat /tmp/all-endpoints | awk -F'<' '{sub(/[(<].*/,""); print $1}' \
        | while read endpoint; \
        do \
            if ! grep -q $endpoint /tmp/react-endpoints;
                then echo $endpoint;
            fi;
    done
}


function trigger-workflow() {
    curl \
        -X POST \
        -H "Accept: application/vnd.github.v3+json" \
        -H "Authorization: token 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" \
        https://api.github.com/repos/gainfulHealth/gainfulWeb/dispatches \
        -d '{"event_type":"on-demand-test","client_payload":{"unit":false,"integration":true}}'
}

function dtest() {
    QUIET=1 venv/bin/python manage.py test -v1 -k --failfast $1 
}

function delete-old-branches() {
    git br --all | grep remotes/origin | cut -d' ' -f3 | sed -e "s/^remotes\/origin\///" \
        | while read branch; do

    author=$(git show -s --format='%ae' origin/$branch)
    echo $author $branch
done
}


function() purge-db() {
    docker-compose -f docker/docker-compose.yml down --remove-orhpans --volumes
    docker-dompose -f docker/docker-compose.yml up --detach
    echo "waiting for db to finish spinning up..."
    sleep 15
    python manage.py createsuperuser --email admin@gainful.com --name admin
}
