.PHONY: pack all run clean
GENERATED = src/parser.mli src/parser.ml src/lexer.ml
OCAMLC = ocamlc # ocamlopt for speed
PACKAGE = parsing

all: main

run:
	./main

src/tree.cmi:
	cd src && $(OCAMLC) -c tree.ml

main.exe: $(GENERATED) src/tree.cmi
	cd src && $(OCAMLC) tree.ml $(GENERATED:src/%=%) main.ml -o ../main.exe

pack: $(GENERATED)
	zip $(PACKAGE) -r Makefile lambda main.ml

$(GENERATED): src/lexer.mll src/parser.mly
	ocamllex src/lexer.mll
	ocamlyacc src/parser.mly

clean:
	cd src && $(DEL) $(GENERATED:src/%=%) *.c* *.o*
	rm -f main $(PACKAGE)
