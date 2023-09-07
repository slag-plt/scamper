dist/ :
	rm -rf dist
	mkdir -p dist/docs
	npm run build
	node dist/index.js
	cp -r web/index.html dist/docs

deploy : dist/
	rsync -rtz dist/docs compsci:csc151.cs.grinnell.edu/scamper

clean :
	rm -rf dist