DEMOS = graph graph-decomposition baskerville-make baskerville-sudoku layout music selection simulation sigil sigil-hats components onion

.PHONY: all clean site $(DEMOS)

all: $(DEMOS)

# Bundle from the workspace root, not from inside the demo directory.
# Relative `path:` dependencies in extraPackages resolve against the current
# working directory, so `cd $@ && spago bundle` silently misresolves them —
# it breaks any demo that depends on a sibling checkout.
$(DEMOS):
	spago bundle -p hylograph-demo-$@

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
