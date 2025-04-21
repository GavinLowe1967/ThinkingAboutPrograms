ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/factorial

all:  $(DIR)/FactorialTest.class

# We need MyTest.class for FactorialTest
$(ROOT)/util/MyTest.class:
	cd ../Testing; make

$(DIR)/FactorialTest.class: $(DIR)/Factorial.class $(ROOT)/util/MyTest.class

$(DIR)/%.class: %.scala
	fsc -d .. -classpath .:.. $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
