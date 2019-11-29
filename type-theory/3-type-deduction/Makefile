.PHONY: pack all run clean
GHC = ghc
PROJECT = Lambda
PARSING = $(PROJECT)/Parse
TOKENIZER = $(PARSING)/Tokens
PARSER = $(PARSING)/ExpressionGrammar
GENERATED = $(PARSER).hs $(TOKENIZER).hs
MAIN = Main
OUT = out

all: $(MAIN)

run:
	./$(MAIN)

$(MAIN): $(GENERATED) $(OUT)
	$(GHC) -Wall -O2 -o $(MAIN) -odir $(OUT) -dumpdir $(OUT) -outputdir $(OUT) -tmpdir $(OUT) $(shell find -type f -name "*.hs")

$(OUT):
	mkdir -p out

pack: $(GENERATED)
	zip $(PROJECT).zip -r Makefile $(PROJECT) $(MAIN).hs

$(GENERATED): $(PARSER).y $(TOKENIZER).x
	alex $(TOKENIZER).x
	happy $(PARSER).y

clean:
	rm -rvf $(GENERATED:%=%) $(OUT)
