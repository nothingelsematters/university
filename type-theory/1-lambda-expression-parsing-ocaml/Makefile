.PHONY: pack all run clean
OCAMLC = ocamlc # ocamlopt for speed?
FOLDER = src
GENERATED = $(FOLDER)/parser.mli $(FOLDER)/parser.ml $(FOLDER)/lexer.ml
MAIN = main

all: $(MAIN)

run:
	./$(MAIN)

$(FOLDER)/lambda.cmi:
	cd $(FOLDER) && $(OCAMLC) -c lambda.ml

$(MAIN): $(GENERATED) $(FOLDER)/lambda.cmi
	cd $(FOLDER) && $(OCAMLC) lambda.ml $(GENERATED:$(FOLDER)/%=%) $(MAIN).ml -o ../$(MAIN)

pack: $(GENERATED)
	zip $(FOLDER) -r Makefile $(FOLDER) $(FOLDER)/$(MAIN).ml

$(GENERATED): $(FOLDER)/lexer.mll $(FOLDER)/parser.mly
	ocamllex $(FOLDER)/lexer.mll
	ocamlyacc $(FOLDER)/parser.mly

clean:
	cd $(FOLDER) && rm -rvf $(GENERATED:$(FOLDER)/%=%) *.c* *.o*
	rm -rvf $(MAIN)
