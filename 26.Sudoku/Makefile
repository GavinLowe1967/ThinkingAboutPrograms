DIR = ../thinkingAboutPrograms/sudoku
CP = .:..

all: $(DIR)/Sudoku.class 

$(DIR)/AdvancedPartial.class: $(DIR)/Partial.class $(DIR)/LegalBitMap.class $(DIR)/PriorityQueue.class

$(DIR)/Sudoku.class:  $(DIR)/AdvancedPartial.class

$(DIR)/%.class: %.scala
	fsc -cp $(CP) -d ..  $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
