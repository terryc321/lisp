

all:	
	rm -f queens.asm 
	rm -f driver driver.o
	cp -v /home/terry/lisp/compiler/step036/driver.c . 
	gcc -o driver.o -c driver.c
	/home/terry/lisp/compiler/compile.sh /home/terry/lisp/demo/queens/queens.scm /home/terry/lisp/demo/queens/queens.asm
	nasm -g -F dwarf -f elf -o queens.o queens.asm
	gcc -g3 -o driver driver.o queens.o
	./driver






























