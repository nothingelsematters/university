GHC = ghc
PROJECTNAME = proofMinimization
MAIN = Main.hs
SOURCES = ProofMinimizator.hs Proof.hs
PREPARE = ExpressionGrammar.hs TaskGrammar.hs Tokens.hs

all: minimization

run:
	./$(PROJECTNAME)

pack:
	zip $(PROJECTNAME).zip $(PREPARE) $(SOURCES) $(MAIN) Makefile

clean:
	rm *.o
	rm *.hi
	rm $(PREPARE)

prepare: $(PREPARE)

minimization: $(PREPARE) $(MAIN) $(SOURCES)
	$(GHC) $(MAIN) -O2 -o $(PROJECTNAME)

$(PREPARE): $(HAPPY) $(ALEX)
	happy TaskGrammar.y
	happy ExpressionGrammar.y
	alex Tokens.x
