

	
	global fac

	;; entry prologue
fac:	mov dword eax , [esp + 4] ; n
	cmp dword eax , 1
	ja fac2		
	mov dword eax , 1
	ret
	
fac2:	dec dword eax
	push dword eax
	call fac
	add dword esp , 4
	mul dword [esp + 4]
	ret
	
	
	
	
