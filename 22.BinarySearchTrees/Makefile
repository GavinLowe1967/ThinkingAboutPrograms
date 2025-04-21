DIR = ../thinkingAboutPrograms/binaryTrees

all:    $(DIR)/BinaryTreeMap.class $(DIR)/BinaryTreeMapRec.class $(DIR)/TreeMapTest.class

CLASSPATH = ./..

$(DIR)/TreeMapTest.class: $(DIR)/BinaryTreeMap.class 

$(DIR)/%.class: %.scala
	fsc -d ..  $<

# -classpath $(CLASSPATH)

clean:
	rm $(DIR)/*.class 
