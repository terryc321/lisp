

all:	
	rm -f -v *.asm *.o cons
	rm -f -v driver driver.o driver.c
	cp -v /home/terry/lisp/compiler/step036/driver.c .

	/home/terry/lisp/compiler/compile.sh /home/terry/lisp/demo/cons/cons.scm /home/terry/lisp/demo/cons/cons.asm
	nasm -g -F dwarf -f elf -o cons.o cons.asm
	gcc -g3 -o driver.o -c driver.c
	gcc -g3 -o cons driver.o cons.o
	./cons



































