GHC = ghc
PROJECTNAME = checkFAProof
MAIN = Main.hs
LOGIC = Logic
PARSING = $(LOGIC)/Parsing
SOURCES = $(LOGIC)/Expression.hs $(LOGIC)/Proof.hs $(LOGIC)/CheckAxiom.hs $(LOGIC)/CheckProof.hs $(PARSING)/ExpressionParser.hs
PREPARE = $(PARSING)/TaskGrammar.hs $(PARSING)/ExpressionGrammar.hs $(PARSING)/Tokens.hs
OUTDIR = out

all: $(PROJECTNAME)

run:
	./$(PROJECTNAME)

noopt: $(PREPARE) $(MAIN) $(SOURCES)
	$(GHC) -Wall $(MAIN) -o $(PROJECTNAME) -odir $(OUTDIR) -dumpdir $(OUTDIR) -outputdir $(OUTDIR) -tmpdir $(OUTDIR)

pack: $(PREPARE)
	zip $(PROJECTNAME).zip $(PREPARE) $(SOURCES) $(MAIN) Makefile

clean:
	rm -f *.o *.hi $(LOGIC)/*.o $(LOGIC)/*.hi $(PREPARE) $(PARSING)/*.o $(PARSING)/*.hi

partclean:
	rm -vf *.o *.hi $(LOGIC)/*.o $(LOGIC)/*.hi

prepare: $(PREPARE)

$(PROJECTNAME): $(PREPARE) $(MAIN) $(SOURCES)
	$(GHC) $(MAIN) -O2 -o $(PROJECTNAME)

$(PREPARE): $(HAPPY) $(ALEX)
	happy $(PARSING)/TaskGrammar.y
	happy $(PARSING)/ExpressionGrammar.y
	alex $(PARSING)/Tokens.x
