ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/maxSegSum

all: 	 $(DIR)/MaxSegSum.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. -deprecation $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
