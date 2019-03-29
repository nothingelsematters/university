GHC = ghc
PROJECTNAME = intuitionisticConversion
MAIN = Main.hs
PARSING = Parsing
SOURCES = IntuitionisticConversion.hs Expression.hs $(PARSING)/ProofParser.hs
PREPARE = $(PARSING)/ExpressionGrammar.hs $(PARSING)/TaskGrammar.hs $(PARSING)/Tokens.hs

all: $(PROJECTNAME)

run:
	./$(PROJECTNAME)
	
noopt: $(PREPARE) $(MAIN) $(SOURCES)
	$(GHC) $(MAIN) -o $(PROJECTNAME)

pack:
	zip $(PROJECTNAME).zip $(PREPARE) $(SOURCES) $(MAIN) Makefile

clean:
	rm -f *.o *.hi $(PREPARE) $(PARSING)/*.o $(PARSING)/*.hi
	
partclean:
	rm -vf *.o *.hi

prepare: $(PREPARE)

$(PROJECTNAME): $(PREPARE) $(MAIN) $(SOURCES)
	$(GHC) $(MAIN) -O2 -o $(PROJECTNAME)

$(PREPARE): $(HAPPY) $(ALEX)
	happy $(PARSING)/TaskGrammar.y
	happy $(PARSING)/ExpressionGrammar.y
	alex $(PARSING)/Tokens.x
