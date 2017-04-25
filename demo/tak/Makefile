

all:
	nasm -f elf -F dwarf -o tak.o tak.asm
	gcc -ggdb -O0 -o test.o -c test.c
	gcc -ggdb -O0 -o ctak test.o tak.o
	./ctak | tee ctak.out

	nasm -f elf -F dwarf -o tak2.o tak2.asm
	gcc -ggdb -O0 -o test.o -c test.c
	gcc -ggdb -O0 -o ctak2 test.o tak2.o
	./ctak2 | tee ctak2.out

# check outputs agree
	diff ctak.out ctak2.out






