DEMOS = graph graph-decomposition jtms-make jtms-sudoku layout music selection simulation sigil sigil-hats

.PHONY: all clean site $(DEMOS)

all: $(DEMOS)

$(DEMOS):
	cd $@ && spago bundle

ISLANDS = hylograph scriptorium reasoning

site: all
	rm -rf docs
	mkdir -p docs
	cp site/index.html docs/
	cp site/archipelago.css docs/
	cp site/tree-hero.jpg docs/
	cp -r site/thumbnails docs/thumbnails
	cp -r site/heroes docs/heroes
	@for island in $(ISLANDS); do \
		cp -r site/$$island docs/$$island; \
	done
	@for demo in $(DEMOS); do \
		cp -r $$demo/public docs/$$demo; \
	done
	@echo "Site assembled in docs/"

clean:
	rm -rf */output */.spago */public/bundle.js docs
