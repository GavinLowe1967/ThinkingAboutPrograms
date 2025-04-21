DIR = ../thinkingAboutPrograms/bitMaps

all:  $(DIR)/BitMapSetTest.class 

IMPLS = $(DIR)/BitMapSet0.class $(DIR)/BitMapSet.class

$(IMPLS): $(DIR)/Set.class

$(DIR)/BitMapSetTest.class: $(IMPLS)

$(DIR)/%.class: %.scala
	fsc -d .. $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
