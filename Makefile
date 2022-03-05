##
## EPITECH PROJECT, 2022
## wolfram
## File description:
## Makefile
##

STACKDIR = $(shell stack path --local-install-root)

TARGET = wolfram

all:
	stack build
	cp $(STACKDIR)/bin/$(TARGET)-exe .
	mv $(TARGET)-exe $(TARGET)

clean:
	stack clean

fclean:
	rm -f $(TARGET)

re: fclean all
