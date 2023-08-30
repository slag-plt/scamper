.PHONY: clean deploy

dist/ : src/version.ts
	mkdir -p dist
	npm run build

src/version.ts :
	npm run gen-version

site/ : dist/
	mkdir -p site/js
	cp -r ide/* site/
	cp -r dist/ide.js site/js/ide.js

deploy : site/
	rsync -rtz site/ compsci:csc151.cs.grinnell.edu/scamper

clean :
	rm -rf dist/ site/ src/version.ts