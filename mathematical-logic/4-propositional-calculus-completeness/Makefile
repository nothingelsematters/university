GHC = ghc
PROJECTNAME = completeProof
MAIN = Main.hs
LOGIC = Logic
PARSING = $(LOGIC)/Parsing
SOURCES = $(LOGIC)/Proofs.hs $(LOGIC)/Deduction.hs $(LOGIC)/CompleteProof.hs $(LOGIC)/Expression.hs $(LOGIC)/Proof.hs $(PARSING)/ExpressionParser.hs
PREPARE = $(PARSING)/ExpressionGrammar.hs $(PARSING)/Tokens.hs

all: $(PROJECTNAME)

run:
	./$(PROJECTNAME)

noopt: $(PREPARE) $(MAIN) $(SOURCES)
	$(GHC) -Wall $(MAIN) -o $(PROJECTNAME) -odir out

pack:
	zip $(PROJECTNAME).zip $(PREPARE) $(SOURCES) $(MAIN) Makefile

clean:
	rm -f *.o *.hi $(LOGIC)/*.o $(LOGIC)/*.hi $(PREPARE) $(PARSING)/*.o $(PARSING)/*.hi

partclean:
	rm -vf *.o *.hi $(LOGIC)/*.o $(LOGIC)/*.hi

prepare: $(PREPARE)

$(PROJECTNAME): $(PREPARE) $(MAIN) $(SOURCES)
	$(GHC) $(MAIN) -O2 -o $(PROJECTNAME)

$(PREPARE): $(HAPPY) $(ALEX)
	happy $(PARSING)/ExpressionGrammar.y
	alex $(PARSING)/Tokens.x
