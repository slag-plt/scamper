.PHONY: site/js

site/ : site/index.html site/reference.html site/docs site/webfonts site/css site/js/

site/js :
	npm run build

site/docs : docs/dist/docs
	mkdir -p site
	cp -r docs/dist/docs site/

site/%.html : ide/%.html
	mkdir -p site
	cp $< $@

site/webfonts : ide/webfonts
	mkdir -p site
	cp -r $< $@

site/css : ide/css
	mkdir -p site
	cp -r $< $@

site/js/ :
	npm run build

deploy : site/
	rsync -rtz site/ compsci:csc151.cs.grinnell.edu/scamper

clean :
	rm -rf dist/ site/ src/version.ts