ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/quicksort

all: 	 $(DIR)/Quicksort.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. -deprecation $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
