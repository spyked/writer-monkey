BASENAME = Main

.PHONY: all clean

all: $(BASENAME)

$(BASENAME): $(BASENAME).hs
	ghc --make $<

clean:
	find . -name "*.hi" -o -name "*.o" | xargs rm -f
	rm -f $(BASENAME)
