ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/exponentiation

all: 	 $(DIR)/ExpTest.class

# We need MyTest.class for ExpTest
$(ROOT)/util/MyTest.class:
	cd ../Testing; make

$(DIR)/ExpTest.class: $(DIR)/SlowExp.class $(DIR)/FastExp.class $(ROOT)/util/MyTest.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. -deprecation $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
