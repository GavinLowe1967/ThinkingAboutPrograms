DIR = ../thinkingAboutPrograms/account

all: $(DIR)/ClubAccount.class 

# Accounts based on linked lists
LL_ACCOUNTS = $(DIR)/LinkedListAccount.class $(DIR)/LinkedListHeaderAccount.class

# All the implementations of Account
ACCOUNTS = $(DIR)/MapAccount.class $(LL_ACCOUNTS) $(DIR)/ArraysAccount.class

$(ACCOUNTS): $(DIR)/Account.class

$(DIR)/ClubAccount.class: $(ACCOUNTS) $(DIR)/ArraysAccount.class

# Recursively make ArraysAccount
$(DIR)/ArraysAccount.class:
	cd ../18.ImplementingADTs; make

# Recursively make linked-list accounts
$(LL_ACCOUNTS):
	cd ../19.LinkedLists; make

CLASSPATH = ..

$(DIR)/%.class: %.scala
	fsc -deprecation -d .. -classpath $(CLASSPATH) $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
 
