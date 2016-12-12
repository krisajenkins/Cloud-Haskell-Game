all: dist/index.html

dist/index.html: $(shell find src -type f -name '*.elm') dist
	elm make --warn --output=$@ src/App.elm

dist:
	@mkdir $@
