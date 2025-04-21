DIR = ../thinkingAboutPrograms/countdown

all: $(DIR)/NumbersGame.class  $(DIR)/NumbersGame0.class 

$(DIR)/NumbersGame.class $(DIR)/NumbersGame0.class: $(DIR)/Expression.class

$(DIR)/WordGame.class: $(DIR)/Trie.class

$(DIR)/%.class: %.scala
	fsc -d .. $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
