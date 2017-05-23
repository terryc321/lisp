

all:
	rm -v -f *.o dummy
	gcc -ggdb -o dummy.o -c dummy.c
	nasm -F dwarf -f elf -o bitmap.o bitmap.asm
	nasm -F dwarf -f elf -o heap.o heap.asm
	gcc -ggdb -o dummy dummy.o heap.o bitmap.o
	./dummy







