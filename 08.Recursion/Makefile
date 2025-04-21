ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/recursion

all: 	 $(DIR)/Factorial.class $(DIR)/Exp.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. -deprecation $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
