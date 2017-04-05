

all:	
	rm -f driver
	nasm -g -F dwarf -f elf -o entry.o entry.asm
	gcc -g3 -o driver.o -c driver.c
	gcc -g3 -o driver driver.o entry.o
	gcc -fomit-frame-pointer -O0 -o test test.c

	nasm -g -F dwarf -f elf -o pair.o pair.asm
	gcc -g3 -o pair driver.o pair.o


	nasm -g -F dwarf -f elf -o pair2.o pair2.asm
	gcc -g3 -o pair2 driver.o pair2.o

	nasm -g -F dwarf -f elf -o pair3.o pair3.asm
	gcc -g3 -o pair3 driver.o pair3.o

	nasm -g -F dwarf -f elf -o pair4.o pair4.asm
	gcc -g3 -o pair4 driver.o pair4.o

	nasm -f elf -F dwarf -g -o pair5.o pair5.asm
	gcc -g3 -o pair5 driver.o pair5.o




















