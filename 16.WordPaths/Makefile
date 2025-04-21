DIR = ../thinkingAboutPrograms/wordPaths

all:	$(DIR)/WordPaths.class

# $(DIR)/WordPaths.class: $(DIR)/WordPathFinder.class

$(DIR)/%.class: %.scala
	fsc -d .. -deprecation -classpath .:.. $<

clean:
	rm $(DIR)/*.class; fsc -shutdown

