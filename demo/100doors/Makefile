

all:	
	rm -f 100doors.asm
	rm -f driver
	/home/terry/lisp/compile.sh /home/terry/lisp/demo/100doors/100doors.scm /home/terry/lisp/demo/100doors/100doors.asm 
	nasm -g -F dwarf -f elf -o 100doors.o 100doors.asm
	gcc -g3 -o driver.o -c driver.c
	gcc -g3 -o driver driver.o 100doors.o
	./driver























