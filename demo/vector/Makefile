

all:	
	rm -f 100doors.asm
	rm -f driver
	cp -v /home/terry/lisp/compiler/step036/driver.c .
	/home/terry/lisp/compiler/compile.sh /home/terry/lisp/demo/vector/vector.scm /home/terry/lisp/demo/vector/vector.asm
	nasm -g -F dwarf -f elf -o vector.o vector.asm
	gcc -g3 -o driver.o -c driver.c
	gcc -g3 -o driver driver.o vector.o
	./driver






























