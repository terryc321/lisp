

all:	
	rm -f queens.asm 
	rm -f driver driver.o
	cp -v /home/terry/lisp/compiler/step036/driver.c . 
	gcc -o driver.o -c driver.c
	/home/terry/lisp/compiler/compile.sh /home/terry/lisp/demo/queens/queens.scm /home/terry/lisp/demo/queens/queens.asm
	nasm -g -F dwarf -f elf -o queens.o queens.asm
# gcc -g3 -o driver.o -c driver.c
	gcc -g3 -o driver driver.o queens.o
	./driver

#cp -v /home/terry/lisp/compiler/step034/driver.c .
#/home/terry/lisp/compile.sh /home/terry/lisp/demo/queens/test.scm /home/terry/lisp/demo/queens/test.asm
#nasm -g -F dwarf -f elf -o test.o test.asm
#gcc -g3 -o driver.o -c driver.c
#gcc -g3 -o driver driver.o test.o
#./driver

































