

	

	global scheme_car
	global scheme_cdr
	

scheme_car: nop
	mov dword eax, [ esp + 4 ]
	dec dword eax
	mov dword eax , [ eax ]
	ret
	
scheme_cdr: nop
	mov dword eax, [ esp + 4 ]
	add dword eax , 3
	mov dword eax , [ eax ] 
	ret

	
	

	

	
	

	

	


