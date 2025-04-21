ROOT = ../thinkingAboutPrograms

# Directory for Accounts
ACC_DIR = $(ROOT)/account

# The Account classes
ACCOUNTS = $(ACC_DIR)/LinkedListAccount.class	\
  $(ACC_DIR)/LinkedListHeaderAccount.class

# Directory for interfaces.
INT_DIR = $(ROOT)/interfaces

# Directory for maps.
MAP_DIR = $(ROOT)/map

MAPS = $(MAP_DIR)/LinkedListHeaderMap.class $(MAP_DIR)/MapTest.class

# Directory for stacks.
STACK_DIR = $(ROOT)/stack

all: $(ACCOUNTS) $(MAPS) $(STACK_DIR)/LinkedListStack.class

CLASSPATH = ..

##### Accounts

$(ACCOUNTS): $(ACC_DIR)/Account.class

# Recursively make the Account trait
$(ACC_DIR)/Account.class:
	cd ../15.Account; make

$(ACC_DIR)/%.class: %.scala
	fsc -deprecation -d .. -classpath $(CLASSPATH) $<

##### Maps

$(MAP_DIR)/%.class: %.scala $(INT_DIR)/Map.class
	fsc -deprecation -d ..  -classpath $(CLASSPATH) $<

# Recursively make the Map trait
$(INT_DIR)/Map.class: Map.scala
	fsc -deprecation -d ..  -classpath $(CLASSPATH) Map.scala

##### Stack

$(INT_DIR)/Stack.class:
	fsc -deprecation -d ..  -classpath $(CLASSPATH) Stack.scala

$(STACK_DIR)/%.class: %.scala $(INT_DIR)/Stack.class
	fsc -deprecation -d ..  -classpath $(CLASSPATH) $<


clean:
	rm $(ACCOUNTS) $(INT_DIR)/*.class $(MAPS) $(STACK_DIR)/LinkedListStack.class; fsc -shutdown

