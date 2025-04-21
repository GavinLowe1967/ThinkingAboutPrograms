ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/binarySearch

all: 	 $(DIR)/SquareRoots.class $(DIR)/ArraySearch.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. -deprecation $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
