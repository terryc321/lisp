

	
	global fac

	;; entry prologue
fac:	push dword ebp
	mov dword ebp , esp

	mov dword eax , [ebp + 8] ; n
	cmp dword eax , 1
	ja fac2		
	mov dword eax , 1
	jmp fac_exit
	
fac2:	dec dword eax
	push dword eax
	call fac
	add dword esp , 4
	mul dword [ebp + 8]
	

	;; restore stack
fac_exit:	mov dword esp , ebp
	pop dword ebp
	ret
	

	
