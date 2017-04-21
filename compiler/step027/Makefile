

all:	
	rm -f driver
	nasm -g -F dwarf -f elf -o entry.o entry.asm
	nasm -g -F dwarf -f elf -o c-help.o c-help.asm
	gcc -g3 -o driver.o -c driver.c
	gcc -g3 -o driver driver.o entry.o c-help.o
















