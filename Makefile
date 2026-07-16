DEMOS = graph graph-decomposition jtms-sudoku layout music selection simulation sigil sigil-hats

.PHONY: all clean site $(DEMOS)

all: $(DEMOS)

$(DEMOS):
	cd $@ && spago bundle

site: all
	rm -rf docs
	mkdir -p docs
	cp site/index.html docs/
	cp -r site/thumbnails docs/thumbnails
	@for demo in $(DEMOS); do \
		cp -r $$demo/public docs/$$demo; \
	done
	@echo "Site assembled in docs/"

clean:
	rm -rf */output */.spago */public/bundle.js docs
