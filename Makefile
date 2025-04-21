ROOT = thinkingAboutPrograms

all:
	cd Testing; make
	cd 01.Chap1; make
	cd 02.Chap2; make
	cd 03.Exponentiation; make
	cd 05.ArraySearch; make
	cd 06.SquareRoots; make
	cd 07.Polynomial; make
	cd 08.Recursion; make
	cd 09.BinarySearch; make
	cd 10.SelectionSort; make
	cd 11.Quicksort; make
	cd 12.Knapsack; make
	cd 13.MaxSegSum; make
	cd 14.SpellCheck; make
	cd 15.Account; make
	cd 16.WordPaths; make
	cd 17.CountdownNumbers; make
	cd 18.ImplementingADTs; make
	cd 19.LinkedLists; make
	cd 20.BitMaps; make
	cd 21.HashTables; make
	cd 22.BinarySearchTrees; make
	cd 23.FindingShortestPaths; make
	cd 24.BinaryHeaps; make
	cd 25.CountdownLetters; make
	cd 26.Sudoku; make

clean:	
	rm $(ROOT)/*/*.class; \
	# rm $(ROOT)/util/*.class; rm $(ROOT)/interfaces/*.class; \
	# rm $(ROOT)/factorial/*.class; \
	# rm $(ROOT)/arraySum/*.class; rm $(ROOT)/exponentiation/*.class;  \
	# rm $(ROOT)/arraySearch/*.class; rm $(ROOT)/squareRoots/*.class; \
	# rm $(ROOT)/polynomial/*.class; rm $(ROOT)/recursion/*.class; \
	# rm $(ROOT)/binarySearch/*.class; rm $(ROOT)/selectionSort/*.class; \
	# rm $(ROOT)/quicksort/*.class; rm $(ROOT)/dynamicProgramming/*.class; \
	# rm $(ROOT)/maxSegSum/*.class; rm $(ROOT)/dictionary/*.class; \
	# rm $(ROOT)/spellCheck/*.class; rm $(ROOT)/account/*.class; \
	# rm $(ROOT)/wordPaths/*.class; rm $(ROOT)/countdown/*.class; \
	# rm $(ROOT)/bitMaps/*.class; rm $(ROOT)/hashMap/*.class; \
	# rm $(ROOT)/map/*.class; rm $(ROOT)/stack/*.class; \
	# rm $(ROOT)/binaryTrees/*.class; rm $(ROOT)/heaps/*.class; \
	# rm $(ROOT)/graphSearch/*.class; rm $(ROOT)/sudoku/*.class; \
	fsc -shutdown
