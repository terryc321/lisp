

	
	global fib

	;; entry prologue
fib:	push dword ebp
	mov dword ebp , esp

	mov dword eax , [ebp + 8] ; n
	cmp dword eax , 0
	je fib0
	cmp dword eax , 2
	ja fib2		
	mov dword eax , 1
	jmp fib_exit

fib0:	mov dword eax , 0
	jmp fib_exit
	
fib2:	dec dword eax
	
	push dword eax		; n - 1
	call fib		; fib n - 1
	add dword esp , 4

	push dword eax

	mov dword eax , [ebp + 8] ; n
	dec dword eax
	dec dword eax
	
	push dword eax
	call fib		; fib n - 2
	add dword esp , 4
		
	add dword eax , [esp]

	;; restore stack
fib_exit:	mov dword esp , ebp
	pop dword ebp
	ret




	

	

	
