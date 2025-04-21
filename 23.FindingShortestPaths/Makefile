ROOT = ../thinkingAboutPrograms

# Main directory for class files
DIR = $(ROOT)/graphSearch

# Directory for PrioriryQueue.class
PQDIR = $(ROOT)/interfaces

# Directory for HeapPrioriyQueue.class
HDIR = $(ROOT)/heaps

all: $(DIR)/Test.class 

$(DIR)/Dijkstra.class: $(DIR)/Graph.class $(HDIR)/HeapPriorityQueue.class

$(DIR)/AStar.class: $(DIR)/Graph.class $(HDIR)/HeapPriorityQueue.class

$(DIR)/Test.class:  $(DIR)/Dijkstra.class $(DIR)/AStar.class

$(HDIR)/HeapPriorityQueue.class: ../24.BinaryHeaps/HeapPriorityQueue.scala $(PQDIR)/PriorityQueue.class 
	cd ../24.BinaryHeaps; make

$(DIR)/%.class: %.scala
	fsc -d ..  -cp .:.. $<

$(PQDIR)/%.class: %.scala
	fsc -d ..  -cp .:.. $<

clean:
	rm $(DIR)/*.class $(HDIR)/HeapPriorityQueue.class $(PQDIR)/PriorityQueue.class ; fsc -shutdown
