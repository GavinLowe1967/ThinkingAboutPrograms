DIR = ../thinkingAboutPrograms/countdown

all: $(DIR)/WordGame.class 

$(DIR)/WordGame.class: $(DIR)/Trie.class

$(DIR)/%.class: %.scala
	fsc -d .. $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
