DEMOS = graph graph-decomposition baskerville-make baskerville-sudoku layout music selection simulation sigil sigil-hats components layout-components onion state-machine force-attractors force-territories

.PHONY: all clean site $(DEMOS)

all: $(DEMOS)

# Bundle from the workspace root, not from inside the demo directory.
# Relative `path:` dependencies in extraPackages resolve against the current
# working directory, so `cd $@ && spago bundle` silently misresolves them —
# it breaks any demo that depends on a sibling checkout.
$(DEMOS):
	spago bundle -p hylograph-demo-$@

# The five machine artifacts are the LIBRARY's conformance corpus — the same
# files its test suite drives — and the demo shows copies of them. Copying in
# one direction only is what stops the demo and the tests disagreeing about
# what a machine says.
state-machine: sync-machines
.PHONY: sync-machines
sync-machines:
	rm -rf state-machine/public/machines
	cp -r ../purescript-glassbox/core/machines state-machine/public/machines

# The force kernel is built (wasm-pack, --target web) in the showcases repo;
# the demo serves a copy, so that public/ is everything a server needs.
FORCE_KERNEL = ../../purescript-hylograph-showcases/wasm-force-demo/pkg
force-attractors force-territories: sync-kernel
.PHONY: sync-kernel
sync-kernel:
	@for demo in force-attractors force-territories; do \
		mkdir -p $$demo/public/pkg; \
		cp $(FORCE_KERNEL)/force_kernel.js $(FORCE_KERNEL)/force_kernel_bg.wasm $$demo/public/pkg/; \
	done

# Rebuild ONE demo and refresh only its copy under docs/. The `site` target
# below is the right thing for a release — it wipes docs/ and rebuilds all of
# them — but that is far too heavy to sit in an edit/reload loop on a single
# demo, and doing the copy by hand is how docs/ drifts from the bundle.
#
#   make refresh-state-machine
#
refresh-%:
	$(MAKE) $*
	rm -rf docs/$*
	cp -r $*/public docs/$*
	@echo "docs/$* refreshed — http://localhost:3005/$*/"

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
