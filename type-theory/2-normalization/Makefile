.PHONY: all compile run pack clean out

KTC = kotlinc
KTR = kotlin
PROJECTNAME = parsing
SOURCES = $(shell find -type f -name "*.kt")
CLASSES = $(patsubst %.kt,out/%.class,$(SOURCES))
DEPS = lib/better-parse-0.3.2.jar
MAINCLASS = MainKt
OUT = out

all: out compile

run:
	${KTR} -cp ${OUT}:${DEPS} -Djava.io.tmpdir=tmp ${MAINCLASS}

pack:
	zip $(PROJECTNAME).zip -r Makefile lambda lib ${MAINCLASS}.kt

clean:
	rm -rvf ${OUT} tmp

compile: ${SOURCES}
	${KTC} -cp ${DEPS}:. -d ${OUT} $^

out:
	mkdir -p ${OUT}
