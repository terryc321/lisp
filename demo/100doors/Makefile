

all:	
	rm -f 100doors.asm
	rm -f driver
	cp -v /home/terry/lisp/compiler/step034/driver.c .
	/home/terry/lisp/compile.sh /home/terry/lisp/demo/100doors/100doors.scm /home/terry/lisp/demo/100doors/100doors.asm
	/home/terry/lisp/compile.sh /home/terry/lisp/demo/100doors/1000doors.scm /home/terry/lisp/demo/100doors/1000doors.asm
	/home/terry/lisp/compile.sh /home/terry/lisp/demo/100doors/100000doors.scm /home/terry/lisp/demo/100doors/100000doors.asm 
	nasm -g -F dwarf -f elf -o 100doors.o 100doors.asm
	nasm -g -F dwarf -f elf -o 1000doors.o 1000doors.asm
	nasm -g -F dwarf -f elf -o 100000doors.o 100000doors.asm

	gcc -g3 -o driver.o -c driver.c
	gcc -g3 -o 100doors driver.o 100doors.o
	gcc -g3 -o 1000doors driver.o 1000doors.o
	gcc -g3 -o 100000doors driver.o 100000doors.o
	./100doors
	./1000doors
	./100000doors





























