

all:	
	rm -f 100doors.asm
	rm -f driver
	cp -v /home/terry/lisp/compiler/step034/driver.c .
	/home/terry/lisp/compile.sh /home/terry/lisp/demo/100doors/100doors.scm /home/terry/lisp/demo/100doors/100doors.asm
	/home/terry/lisp/compile.sh /home/terry/lisp/demo/100doors/1000doors.scm /home/terry/lisp/demo/100doors/1000doors.asm
	/home/terry/lisp/compile.sh /home/terry/lisp/demo/100doors/5000doors.scm /home/terry/lisp/demo/100doors/5000doors.asm
	/home/terry/lisp/compile.sh /home/terry/lisp/demo/100doors/10000doors.scm /home/terry/lisp/demo/100doors/10000doors.asm
	/home/terry/lisp/compile.sh /home/terry/lisp/demo/100doors/100000doors.scm /home/terry/lisp/demo/100doors/100000doors.asm 
	nasm -g -F dwarf -f elf -o 100doors.o 100doors.asm
	nasm -g -F dwarf -f elf -o 1000doors.o 1000doors.asm
	nasm -g -F dwarf -f elf -o 5000doors.o 5000doors.asm
	nasm -g -F dwarf -f elf -o 10000doors.o 10000doors.asm
	nasm -g -F dwarf -f elf -o 100000doors.o 100000doors.asm

	gcc -g3 -o driver.o -c driver.c
	gcc -g3 -o 100doors driver.o 100doors.o
	gcc -g3 -o 1000doors driver.o 1000doors.o
	gcc -g3 -o 5000doors driver.o 5000doors.o

	gcc -g3 -o 10000doors driver.o 10000doors.o
	gcc -g3 -o 100000doors driver.o 100000doors.o
	./100doors | tee 100doors.solution
	./1000doors | tee 1000doors.solution
	./5000doors | tee 5000doors.solution
	./10000doors | tee 10000doors.solution
	./100000doors | tee 100000doors.solution
































