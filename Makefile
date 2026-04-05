DEMOS = graph graph-decomposition layout music selection simulation sigil sigil-hats

.PHONY: all clean $(DEMOS)

all: $(DEMOS)

$(DEMOS):
	cd $@ && spago bundle

clean:
	rm -rf */output */.spago */public/bundle.js
