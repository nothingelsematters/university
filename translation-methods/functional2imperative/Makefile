.PHONY: all run clean

PROJECT = functional2imperative
FOLDER = src
MAIN = $(FOLDER)/main/$(PROJECT)
PARSER = $(MAIN)/parser
OUTDIR = out
GENERATED = $(PARSER)/FunctionalParser.java

ANTLR = /nix/store/nicfx6951mlscczmjmgw3kb5vbb97sqc-antlr-4.7.2/share/java/antlr-4.7.2-complete.jar
ANTLR_SOURCES = $(shell find src -type f -name "*.g4")
ANTLR_RESULTS = $(patsubst %.g4,%Parser.java,$(ANTLR_SOURCES))
ANTLR_CLASSES = $(OUT)/$(PROJECT)/parser/FunctionalParser.class

FUNC_SOURCES = $(MAIN)/Exceptions.kt $(MAIN)/Common.kt $(MAIN)/FunctionalProgram.kt
FUNC_CLASSES = $(OUTDIR)/$(PROJECT)/FunctionalProgramKt.class

TESTS = $(OUT)/$(PROJECT)/TranslatorTestKt.class

SOURCES = $(shell find src -type f -name "*.kt")
CLASSES = $(OUT)/$(PROJECT)/TranslatorKt.class

all: $(CLASSES)

run:
	kotlin -cp $(OUTDIR) $(PROJECT).TranslatorKt

test: $(CLASSES) $(TESTS)
	kotlin -cp $(OUTDIR) $(PROJECT).TranslatorTestKt

$(TESTS): all
	kotlinc -d $(OUTDIR) -cp $(ANTLR):$(OUTDIR) $(shell find src/test -type f -name "*.kt")

$(FUNC_CLASSES): $(OUTDIR) $(FUNC_SOURCES) $(MAIN)/FunctionalProgram.kt
	kotlinc -d $(OUTDIR) -cp $(ANTLR) $(FUNC_SOURCES)

$(ANTLR_CLASSES): $(ANTLR_RESULTS) $(FUNC_CLASSES)
	javac -d $(OUTDIR) -cp $(ANTLR):$(OUTDIR) $(PARSER)/FunctionalBaseListener.java \
		$(PARSER)/FunctionalLexer.java $(PARSER)/FunctionalListener.java \
		$(PARSER)/FunctionalParser.java

$(CLASSES): $(OUTDIR) $(FUNC_CLASSES) $(ANTLR_CLASSES) $(SOURCES)
	kotlinc -d $(OUTDIR) -cp $(ANTLR):$(OUTDIR) \
		$(PARSER)/FunctionalParserUtils.kt \
		$(MAIN)/Exceptions.kt $(MAIN)/ImperativeProgram.kt \
		$(MAIN)/Common.kt $(MAIN)/FunctionalToImperative.kt \
		$(MAIN)/Translator.kt

%Parser.java: %.g4
	java -jar $(ANTLR) $<

clean:
	rm -rvf out
	find $(PARSER) -regextype grep -regex '.*\(java\|interp\|tokens\)' -exec rm -rvf {} \;

$(OUTDIR):
	mkdir -p $(OUTDIR)
