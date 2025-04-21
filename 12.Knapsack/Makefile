ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/dynamicProgramming

all: 	 $(DIR)/Knapsack.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. -deprecation $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
