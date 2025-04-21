ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/arraySum

all: $(DIR)/ArraySum.class $(DIR)/ArraySumTest.class $(DIR)/TestTest.class

# We need MyTest.class for ArraySumTest
$(ROOT)/util/MyTest.class:
	cd ../Testing; make

$(DIR)/ArraySumTest.class: $(DIR)/ArraySum.class $(ROOT)/util/MyTest.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. $<


clean:
	rm $(DIR)/*.class; fsc -shutdown
