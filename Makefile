all:
	elm make --output=app_v$$(cat VERSION).js src/Main.elm
	sed "s/\$$VERSION/$$(cat VERSION)/" index.template > index.html

live:
	elm make --output=test.html src/Main.elm

test: live
	elm reactor

release: all
	sed "s/\$$VERSION/$$(cat VERSION)/" serviceworker.template > calcsw.js

bump:
	echo $$(( $$(cat VERSION) + 1  )) > VERSION

deploy: release
	./deploy.sh

clean:
	rm -rf elm-stuff app_v*.js test.html calcsw.js index.html
