# Demos that build against published registry packages
DEMOS = graph graph-decomposition music selection simulation sigil sigil-hats

# layout demo is blocked on hylograph-layout publish (needs NodeValueStrategy export)
# Uncomment after publishing:
# DEMOS += layout

.PHONY: all clean $(DEMOS)

all: $(DEMOS)

$(DEMOS):
	cd $@ && spago bundle

clean:
	rm -rf */output */.spago */public/bundle.js
