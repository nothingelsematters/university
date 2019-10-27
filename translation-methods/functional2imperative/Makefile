OUTDIR = out
PROJECTDIR = src/main
PARSER = ${PROJECTDIR}/functional2imperative/parser

SOURCES = $(shell find src -type f -name "*.java")
CLASSES = $(patsubst $(PROJECTDIR)/%.java,$(OUTDIR)/%.class,$(SOURCES))
ANTLR_SOURCES = $(shell find src -type f -name "*.g4")
ANTLR_RESULTS = $(patsubst %.g4,%Parser.java,$(ANTLR_SOURCES))

ANTLR = /nix/store/nicfx6951mlscczmjmgw3kb5vbb97sqc-antlr-4.7.2/share/java/antlr-4.7.2-complete.jar
KOTLINSTD = /nix/store/4ba4lrrsnvww4xm936dmvif6mxgs7pir-kotlin-1.3.50/lib/kotlin-stdlib.jar

MAINCLASS = functional2imperative.TranslatorKt


all: $(ANTLR_RESULTS) $(CLASSES)

run:
	java -cp ${OUTDIR}:${ANTLR}:${KOTLINSTD}:${OUTDIR}/functional2imperative:${OUTDIR}/functional2imperative/parser ${MAINCLASS}

clean:
	rm -rvf out
	find ${PARSER} -regextype grep -regex '.*\(java\|interp\|tokens\)' -exec rm -rvf {} \;

%Parser.java: %.g4
	java -jar ${ANTLR} $<

$(CLASSES): $(SOURCES) $(OUTDIR)
	kotlinc -d ${OUTDIR} ${PROJECTDIR}/functional2imperative/Common.kt ${PROJECTDIR}/functional2imperative/FunctionalProgram.kt
	javac -cp ${ANTLR}:${OUTDIR} -d ${OUTDIR} ${PARSER}/FunctionalBaseListener.java ${PARSER}/FunctionalLexer.java ${PARSER}/FunctionalListener.java ${PARSER}/FunctionalParser.java
	kotlinc -cp ${ANTLR}:${OUTDIR}:${OUTDIR}/functional2imperative -d ${OUTDIR} ${PROJECTDIR}/functional2imperative/parser/FunctionalParserUtils.kt ${OUTDIR} ${PROJECTDIR}/functional2imperative/ImperativeProgram.kt ${PROJECTDIR}/functional2imperative/Translator.kt

$(OUTDIR):
	mkdir -p ${OUTDIR}
