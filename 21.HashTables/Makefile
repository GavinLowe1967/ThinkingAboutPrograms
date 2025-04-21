DIR = ../thinkingAboutPrograms/hashMap

all: 	$(DIR)/Test.class

clean: 
	rm $(DIR)/*.class; fsc -shutdown

IMPLS = $(DIR)/HashMap.class $(DIR)/OpenHashMap0.class $(DIR)/OpenHashMap.class

$(DIR)/Test.class: $(IMPLS)

$(DIR)/%.class: %.scala
	fsc -deprecation -d .. $<

