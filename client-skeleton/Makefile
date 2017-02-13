all: .packages \
     dist/index.html

.packages: elm-package.json
	elm package install --yes
	touch $@

dist/index.html: $(shell find src -type f -name '*.elm') dist
	elm make --warn --output=$@ src/App.elm

dist:
	@mkdir $@
