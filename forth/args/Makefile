

all:
	nasm -f elf -F dwarf -o tak.o tak.asm
	gcc -ggdb -O0 -o test.o -c test.c
	gcc -ggdb -O0 -o test test.o tak.o
	./test




