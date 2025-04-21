ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/arraySearch

all: $(DIR)/ArraySearch.class $(DIR)/ArrayEquality.class $(DIR)/SubArray.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. -deprecation $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
