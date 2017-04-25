


;;; TAK in x86 assembler
;;; now NOT using base pointer	
;;; follow C convention of pushing arguments in order right to left

	global tak1
	global tak2
	global tak3
	global tak
	

	
tak1:	mov dword eax , [esp + 4] ; x
	ret

tak2:	mov dword eax , [esp + 8] ; y
	ret

tak3:	mov dword eax , [esp + 12] ; z
	ret

	
tak:	mov dword eax , [esp + 8] ; y
	mov dword ebx , [esp + 4] ; x
	cmp dword eax , ebx
	jb tak_2
	
	mov dword eax , [esp + 12] ; z = done
	jmp tak_exit


	;; TAK : ARG Z
tak_2:  mov dword eax , [esp + 12] ; z 
	dec dword eax 		; z - 1
	mov dword ebx , [esp + 4] ; x
	mov dword ecx , [esp + 8] ; y
	push dword ecx
	push dword ebx
	push dword eax
	call tak
	add dword esp , 12 	; drop z y x-1
	;; tak z-1 x y in EAX register
	push dword eax

	;; TAK : ARG Y
	
	;; x y z all add 4 to get originals
	mov dword eax , [esp + 12] ; y 
	dec dword eax 		; y - 1
	mov dword ebx , [esp + 16] ; z
	mov dword ecx , [esp + 8] ; x
	push dword ecx
	push dword ebx
	push dword eax
	call tak
	add dword esp , 12 	; drop z y x-1
	
	;; tak y-1 z x in EAX register
	push dword eax

	;; TAK : ARG X
	mov dword eax , [esp + 12] ; x 
	dec dword eax 		; x - 1
	mov dword ebx , [esp + 16] ; y
	mov dword ecx , [esp + 20] ; z
	push dword ecx
	push dword ebx
	push dword eax
	call tak
	add dword esp , 12 	; drop z y x-1
	;; tak x-1 y z in EAX register
	push dword eax

	;; TAK : (TAK ARGX)(TAK ARGY)(TAK ARGZ)
	call tak
	add dword esp , 12 	; drop TAKs

	;; result in EAX register

tak_exit:  	ret


	
	
	
	
	
