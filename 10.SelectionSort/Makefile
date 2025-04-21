ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/selectionSort

all: 	 $(DIR)/SelectionSort.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. -deprecation $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
