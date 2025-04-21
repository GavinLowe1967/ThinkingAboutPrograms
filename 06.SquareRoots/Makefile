ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/squareRoots

all: $(DIR)/SquareRoots.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. -deprecation $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
