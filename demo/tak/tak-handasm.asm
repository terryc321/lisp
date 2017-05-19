


;;; TAK in x86 assembler using Base Pointer EBP
;;; follow C convention of pushing arguments in order right to left

	global tak1
	global tak2
	global tak3
	global tak
	

	
tak1:	push dword ebp ; entry prologue
	mov dword ebp , esp
	mov dword eax , [ebp + 8] ; x
	mov dword esp , ebp	  ; exit prolog
	pop dword ebp
	ret

tak2:	push dword ebp ; entry prologue
	mov dword ebp , esp
	mov dword eax , [ebp + 12] ; y
	mov dword esp , ebp	  ; exit prolog
	pop dword ebp
	ret

tak3:	push dword ebp ; entry prologue
	mov dword ebp , esp
	mov dword eax , [ebp + 16] ; z
	mov dword esp , ebp	  ; exit prolog
	pop dword ebp
	ret


	
tak:	push dword ebp
	mov dword ebp , esp
	
	mov dword eax , [ebp + 12] ; y
	mov dword ebx , [ebp + 8] ; x
	cmp dword eax , ebx
	jb tak_2
	
	mov dword eax , [ebp + 16] ; z = done
	jmp tak_exit


	;; TAK : ARG Z
tak_2:  mov dword eax , [ebp + 16] ; z 
	dec dword eax 		; z - 1
	mov dword ebx , [ebp + 8] ; x
	mov dword ecx , [ebp + 12] ; y
	push dword ecx
	push dword ebx
	push dword eax
	call tak
	add dword esp , 12 	; drop z y x-1
	;; tak z-1 x y in EAX register
	push dword eax

	;; TAK : ARG Y
	
	;; x y z all add 4 to get originals
	mov dword eax , [ebp + 12] ; y 
	dec dword eax 		; y - 1
	mov dword ebx , [ebp + 16] ; z
	mov dword ecx , [ebp + 8] ; x
	push dword ecx
	push dword ebx
	push dword eax
	call tak
	add dword esp , 12 	; drop z y x-1
	
	;; tak y-1 z x in EAX register
	push dword eax

	;; TAK : ARG X
	mov dword eax , [ebp + 8] ; x 
	dec dword eax 		; x - 1
	mov dword ebx , [ebp + 12] ; y
	mov dword ecx , [ebp + 16] ; z
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

tak_exit:  	mov dword esp , ebp	  ; exit prolog
	pop dword ebp
	ret


	
	
	
	
	
