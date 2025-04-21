ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/polynomial

all: 	 $(DIR)/Polynomial.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. -deprecation $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
