ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/spellCheck
DDIR = $(ROOT)/dictionary

all: $(DIR)/SpellCheck0.class $(DIR)/SpellCheck.class

$(DIR)/SpellCheck.class: $(DDIR)/Dictionary.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. $<

$(DDIR)/%.class: %.scala
	fsc -d .. -classpath .:.. $<

clean:
	rm $(DIR)/*.class $(DDIR)/*.class; fsc -shutdown
