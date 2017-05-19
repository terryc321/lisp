

	global heap1_addr
	global heap2_addr
	global heap_size
	global heap1_fill

	

	section .text

heap1_addr:	lea dword eax , [heap1]
	ret

heap2_addr:	lea dword eax , [heap2]
	ret

heap_size:	mov dword eax , 400
	ret

	;; clobbers ebx 
heap1_fill:	lea dword eax , [heap1]
	push dword ebx
	push dword ecx
	mov dword ecx , 1
	
heap1_fill2:	cmp dword eax , heap2
	jae heap1_fill_done
	
	mov dword [eax] , ecx
	inc dword ecx
	add dword eax , 4
	jmp heap1_fill2
heap1_fill_done:
	pop dword ecx
	pop dword ebx
	ret
	
	
	section .data

	
dummy:	dd 40
	
	
	section .bss	
heap1:	resd 400	
heap2:	resd 400


	

	
