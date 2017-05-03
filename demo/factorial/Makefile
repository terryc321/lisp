

all:
	nasm -f elf -o fac.o fac.asm
	nasm -f elf -o fac2.o fac2.asm
	gcc -c main.c
	gcc -c cfac.c
	#ld -o fac /lib/crt0.o main.o fac.o -lc
	#ld -o fac main.o fac.o -lc
	gcc -o cfac main.o cfac.o
	gcc -o fac main.o fac.o
	gcc -o fac2 main.o fac2.o
	gcc -O3 -o cfac2 main.o cfac.o











