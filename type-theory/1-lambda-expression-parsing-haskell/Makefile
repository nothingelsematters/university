GHC = ghc
PROJECTNAME = lambda
MAIN = Main.hs
PACKAGE = Lambda
PARSING = $(PACKAGE)/Parse
SOURCES = $(PACKAGE)/Expression.hs $(PARSING)/ExpressionParser.hs
PREPARE = $(PARSING)/ExpressionGrammar.hs $(PARSING)/Tokens.hs
OUTDIR = out

all: $(PROJECTNAME)

run:
	./$(PROJECTNAME)

noopt: $(PREPARE) $(MAIN) $(SOURCES)
	$(GHC) -Wall $(MAIN) -o $(PROJECTNAME) -odir $(OUTDIR) -dumpdir $(OUTDIR) -outputdir $(OUTDIR) -tmpdir $(OUTDIR)

pack: $(PREPARE)
	zip $(PROJECTNAME).zip $(PREPARE) $(SOURCES) $(MAIN) Makefile

clean:
	rm -f *.o *.hi $(PACKAGE)/*.o $(PACKAGE)/*.hi $(PREPARE) $(PARSING)/*.o $(PARSING)/*.hi $(PROJECTNAME)
	rm -rvf $(OUTDIR)

partclean:
	rm -vf *.o *.hi $(PACKAGE)/*.o $(PACKAGE)/*.hi

prepare: $(PREPARE)

$(PROJECTNAME): $(PREPARE) $(MAIN) $(SOURCES)
	$(GHC) $(MAIN) -O2 -o $(PROJECTNAME)

$(PREPARE): $(HAPPY) $(ALEX)
	happy $(PARSING)/ExpressionGrammar.y
	alex $(PARSING)/Tokens.x
