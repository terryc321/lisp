

all:
	rm -v -f dummy.o heap.o dummy
	gcc -ggdb -o dummy.o -c dummy.c
	nasm -F dwarf -f elf -o heap.o heap.asm
	gcc -ggdb -o dummy dummy.o heap.o
	./dummy





