DIR = ../thinkingAboutPrograms/util

all: $(DIR)/MyTest.class $(DIR)/TestTest.class

TestTest.class:	MyTest.class

$(DIR)/%.class: %.scala
	fsc -d .. $<

# %.class: %.scala
# 	fsc -d .. -classpath .:.. $<	

clean:
	rm $(DIR)/*.class; fsc -shutdown
