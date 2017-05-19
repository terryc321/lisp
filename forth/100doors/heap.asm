

	global heap1_addr
	global heap_size
	global heap1_fill
	global toggleEvery
	global toggleAll

	
	;; 1 = door open
	;; 0 = door closed

	section .text

heap1_addr:	lea dword eax , [heap1]
	ret


heap_size:	mov dword eax , [dummy]
	ret


	
	
;;; toggle every n doors
;;; 
;;; revision
;;; version 2 )
;;; 
;;; version 1 )
;;; initially had counter . when it decremented to 1 would toggle door (not X).
toggleEvery:	push dword ebp
	mov dword ebp , esp
	
	push dword ebx
	push dword ecx
	
	mov dword ecx , [ebp + 8]
	lea dword eax , [heap1 + ecx * 4]
	
	;; check not past end of array
toggleEveryLoop:	cmp dword eax , heap1End 
	jae toggleEveryDone

;; 	cmp dword ecx , 1
;; 	jbe toggleEveryHit

;; 	;; 
;; toggleEveryMiss:	nop
;; 	add dword eax , 4
;; 	dec dword ecx
;; 	jmp toggleEveryLoop	

toggleEveryHit:		nop
	not dword [eax]
	add dword eax , ecx
	jmp toggleEveryLoop		

toggleEveryDone:	pop dword ecx
	pop dword ebx

	mov dword esp , ebp
	pop dword ebp
	ret



;;; loops from 1 to 100 calls toggleEvery
toggleAll:	
	push dword ebp
	mov dword ebp , esp

	mov dword eax , 1
toggleAllLoop:	nop
	push dword eax
	call toggleEvery
	pop dword eax
	inc dword eax
	cmp dword eax , 100
	ja toggleAllDone
	jmp toggleAllLoop

toggleAllDone:
	mov dword esp , ebp
	pop dword ebp
	ret
		


	
	section .data

	
dummy:	dd 100
	
	
	section .bss	
heap1:	resd 400	
heap1End:	resd 4

	



	

	
