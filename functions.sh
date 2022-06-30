function :e() {
	vim ~/.functions.sh
	. ~/.functions.sh
}

function venv() {
    . venv/bin/activate
}

function unused-endpoints() {
    awk -F '[`"]' '{sub(/[\?\$].*/,""); $2 != ""; if ($2) print $2}' reactjs/src/api/endpoints.ts > /tmp/react-endpoints

	QUIET=1 DJANGO_SETTINGS_MODULE=settings.development ~/gainfulWeb/venv/bin/python ~/gainfulWeb/manage.py show_urls --format=pretty-json | jq -r 'map(select(.module | startswith("api") or startswith("blog") or startswith("consultation") or startswith("ebdjango") or startswith("influencers") or startswith("profiles") or startswith("reviews") or startswith("sms")) | .url) | sort | .[]' > /tmp/all-endpoints

    cat /tmp/all-endpoints | awk -F'<' '{sub(/[(<].*/,""); print $1}' \
        | while read endpoint; \
        do \
            if ! grep -q $endpoint /tmp/react-endpoints;
                then echo $endpoint;
            fi;
    done
}
