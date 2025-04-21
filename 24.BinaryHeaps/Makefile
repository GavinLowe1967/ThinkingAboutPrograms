ROOT = ../thinkingAboutPrograms
DIR = $(ROOT)/heaps

# Directory for PrioriryQueue.class
PQDIR = $(ROOT)/interfaces

# Directory for HeapPrioriyQueue.class
#HDIR = $(ROOT)/heaps

all: $(DIR)/HeapSortTest.class $(DIR)/HeapPriorityQueue.class

$(DIR)/HeapSortTest.class: $(DIR)/HeapSort.class

$(HDIR)/HeapPriorityQueue.class: HeapPriorityQueue.scala $(PQDIR)/PriorityQueue.class
	fsc -d .. -deprecation HeapPriorityQueue.scala

$(PQDIR)/PriorityQueue.class: ../23.FindingShortestPaths/PriorityQueue.scala
	cd ../23.FindingShortestPaths; make

$(DIR)/%.class: %.scala
	fsc -d .. -deprecation $<

clean:
	rm $(DIR)/*.class $(PQDIR)/PriorityQueue.class; fsc -shutdown
