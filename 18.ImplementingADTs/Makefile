DIR = ../thinkingAboutPrograms/account

# This directory contains just ArraysAccount.scala and ArraysTest.scala, but
# has to recursively make files in other directories.

all:  $(DIR)/AccountTest.class

# Accounts based on linked lists
LL_ACCOUNTS =  $(DIR)/LinkedListAccount.class $(DIR)/LinkedListHeaderAccount.class

OTHER_ACCOUNTS = $(DIR)/MapAccount.class $(LL_ACCOUNTS)

# Recursively make the trait and the other accounts.
$(DIR)/Account.class $(DIR)/MapAccount.class:
	cd ../15.Account; make

# Recursively make linked-list accounts
$(LL_ACCOUNTS):
	cd ../19.LinkedLists; make

$(DIR)/ArraysAccount.class: $(DIR)/Account.class

$(DIR)/AccountTest.class: $(OTHER_ACCOUNTS) $(DIR)/ArraysAccount.class

CLASSPATH = ..

$(DIR)/%.class: %.scala
	fsc -deprecation -d .. -classpath $(CLASSPATH) $<

clean:
	rm $(DIR)/*.class; fsc -shutdown
