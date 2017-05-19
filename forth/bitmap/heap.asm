

;;; **********************************************	
	global heap1_addr
	global heap2_addr
	global bitmap1_addr
	global bitmap2_addr
	global heap_size
	
	global heap_clear
	global heap_fill

	
;;; **********************************************
	section .text


bitmap1_addr:	lea dword eax , [bitmap1]
	ret

bitmap2_addr:	lea dword eax , [bitmap2]
	ret
	
	
heap1_addr:	lea dword eax , [heap1]
	ret

heap2_addr:	lea dword eax , [heap2]
	ret

heap_size:	mov dword eax , heapsize
	ret



	

;;; ********* clear the heap **********
;;; clearing heap 4 bytes a double word , at a time 
;;; lea dword eax , [heap1]	
heap_clear:
	mov dword eax , [esp + 4]
	push dword ebx
	push dword edx

	lea dword ebx , [eax + heapsize * 4] 
	
heap_clear_loop:	cmp dword eax , ebx
	jae heap_clear_done
	
	mov dword [eax] , 0
	add dword eax , 4
	jmp heap_clear_loop
	
heap_clear_done:
	pop dword edx
	pop dword ebx
	ret


	
;;; ********* fill the heap **********
;;; clearing heap 4 bytes a double word , at a time 
;;; lea dword eax , [heap1]	
heap_fill:
	mov dword eax , [esp + 4]
	push dword ebx
	push dword edx

	lea dword ebx , [eax + heapsize * 4] 
	
heap_fill_loop:	cmp dword eax , ebx
	jae heap_fill_done
	
	mov dword [eax] , -1
	add dword eax , 4
	jmp heap_fill_loop
	
heap_fill_done:
	pop dword edx
	pop dword ebx
	ret

	
;;; **********************************************
	section .data	
dummy:	dd 40
heapsize:	equ (32 * 100)

	
	
	
;;; **********************************************	
	section .bss
	;; on 32 bit system a multiple of 32 for bitmap
heap1:		resd heapsize
heap2:		resd heapsize
bitmap1:	resd  (heapsize / 32)
bitmap2:	resd  (heapsize / 32)

	
	


	
