

all:	
	rm -f driver
	nasm -F dwarf -f elf -o entry.o entry.asm
	gcc -g3 -o driver.o -c driver.c
	gcc -g3 -o driver driver.o entry.o














