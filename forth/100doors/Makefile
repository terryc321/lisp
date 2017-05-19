

all:
	rm -v -f dummy1.o dummy2.o dummy.o heap.o dummy doors1.o doors2.o doors1 doors2
	gcc -ggdb -o dummy1.o -c dummy1.c
	gcc -ggdb -o dummy2.o -c dummy2.c
	nasm -F dwarf -f elf -o doors1.o doors1.asm
	nasm -F dwarf -f elf -o doors2.o doors2.asm
	gcc -ggdb -o doors1 doors1.o dummy1.o
	gcc -ggdb -o doors2 doors2.o dummy2.o
	time ./doors1
	time ./doors2









