

all:	
	rm -f 100doors.asm
	rm -f driver
	/home/terry/lisp/compile.sh /home/terry/lisp/demo/let/let.scm /home/terry/lisp/demo/let/let.asm
	nasm -g -F dwarf -f elf -o let.o let.asm
	gcc -g3 -o driver.o -c driver.c
	gcc -g3 -o driver driver.o let.o
	./driver




























