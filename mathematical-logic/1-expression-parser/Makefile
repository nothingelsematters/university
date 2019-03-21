GHC = ghc
ALEX = Tokens.x
HAPPY = Grammar.y
SOURCES = Main.hs Expression.hs
GENERATED = Tokens.hs Grammar.hs
OUTNAME = expressionParser

all: parser

run:
	./$(OUTNAME)

pack: $(GENERATED)
	zip $(OUTNAME).zip $(SOURCES) Makefile

clean:
	rm *.o
	rm *.hi
	rm $(GENERATED)

parser: $(GENERATED) $(SOURCES)
	$(GHC) Main.hs -o $(OUTNAME)
	$(clean)

$(GENERATED): $(ALEX) $(HAPPY) $(SOURCES)
	alex $(ALEX)
	happy $(HAPPY)
