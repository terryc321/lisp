

;;; **********************************************	

	global bitmap_clear
	global bitmap1_addr
	global bitmap2_addr
	global bitmap_fill

	global bitset
	global bitclr
	global bittst
	
;;; **********************************************
	section .text


bitmap1_addr:	lea dword eax , [bitmap1]
	ret

bitmap2_addr:	lea dword eax , [bitmap2]
	ret
	

;;; ********* test if bit in the bitmap is set **********
;;;  returns 1 or 0 in EAX register
;;; 
;;;  [ C stack : after 3 pushes and 2 formal args ]
;;;  n      <- esp + 20
;;;  bitmap <- esp + 16
;;;  ret-ip <- esp + 12
;;;  edx    <- esp + 8
;;;  ebx    <- esp + 4
;;;  ecx    <- esp + 0
;;; *******************************************
;;; 

	
;;; divide 
;;; edx : eax divided by r/m32
;;; quotient eax , remainder edx
bittst:	push dword edx
	push dword ebx
	push dword ecx
	
	mov dword eax , [esp + 20]
	mov dword edx , 0
	mov dword ebx , 32
	div dword ebx
	;; edx : eax / ebx has 32

	;; quotient in eax
	;; remainder in edx
	;; lea dword ebx , [eax * 4 + bitmap]
	;; lea dword ebx , [eax * 4]
	
	shl dword eax , 2 	; multiply eax by 4 : shift left twice
	mov dword ebx , eax
	add dword ebx , [esp + 16]
	
	mov dword eax , [ebx]

	;; bit test 
	bt dword eax , edx
	;; carry flag has

	;; xor dword eax , eax ; clobbers carry flag -- no good.
	mov dword eax , 0
	;; set if carry flag set
	setc al
		
	pop dword ecx
	pop dword ebx
	pop dword edx
	ret


	
;;; ********* set bit in the bitmap **********
;;; 
;;;  [ C stack : after 3 pushes and 2 formal args ]
;;;  n      <- esp + 20
;;;  bitmap <- esp + 16
;;;  ret-ip <- esp + 12
;;;  edx    <- esp + 8
;;;  ebx    <- esp + 4
;;;  ecx    <- esp + 0
;;; *******************************************
	
;;; divide 
;;; edx : eax divided by r/m32
;;; quotient eax , remainder edx
bitset:	push dword edx
	push dword ebx
	push dword ecx
	
	mov dword eax , [esp + 20]
	mov dword edx , 0
	mov dword ebx , 32
	div dword ebx
	;; edx : eax / ebx has 32

	;; quotient in eax
	;; remainder in edx
	;; lea dword ebx , [eax * 4 + bitmap]
	;; lea dword ebx , [eax * 4]
	
	shl dword eax , 2 	; multiply eax by 4 : shift left twice
	mov dword ebx , eax
	add dword ebx , [esp + 16]
	
	mov dword eax , [ebx]

	;; bit test and set
	bts dword eax , edx

	mov dword [ebx] , eax

	pop dword ecx
	pop dword ebx
	pop dword edx
	ret


	
	
;;; divide 
;;; edx : eax divided by r/m32
;;; quotient eax , remainder edx
bitclr:	push dword edx
	push dword ebx
	push dword ecx
	
	mov dword eax , [esp + 20]
	mov dword edx , 0
	mov dword ebx , 32
	div dword ebx
	;; edx : eax / ebx has 32

	;; quotient in eax
	;; remainder in edx
	;; lea dword ebx , [eax * 4 + bitmap]
	lea dword ebx , [eax * 4]
	add dword ebx , [esp + 16]
 	mov dword eax , [ebx]
	
	;; bit test and reset	
	btr dword eax , edx

	mov dword [ebx] , eax

	pop dword ecx
	pop dword ebx
	pop dword edx
	ret

	
	
	
;;; ********* clear the bitmap **********
;;; able to clear all 32 bits in one register WRITE .
;;; clearing heap 4 bytes a double word , at a time
;;; [ C Stack : 2 formal args : bitmap_clear(BITMAP_ADDR , SIZE) ]
;;; size n      <- esp + 16
;;; bitmap      <- esp + 12
;;; ret-ip 	<- esp + 8
;;; ebx		<- esp + 4
;;; edx		<- esp
;;; *************************************
	
bitmap_clear:	push dword ebx
	push dword edx

	;;  size - multiply by 4 convert to address add to bitmap base
	mov dword eax , [esp + 16] ; size	
	shl dword eax , 2	   ; multiply by 4 ie shift left twice
	mov dword ebx , eax
		
	mov dword eax , [esp + 12]
	add dword ebx , eax
		
	;; lea dword ebx , [eax + bitmapsize * 4] 
	
bitmap_clear_loop:	cmp dword eax , ebx
	jae bitmap_clear_done
	
	mov dword [eax] , 0
	add dword eax , 4
	jmp bitmap_clear_loop
	
bitmap_clear_done:
	pop dword edx
	pop dword ebx
	ret

	

	
;;; ********* fill the bitmap **********
bitmap_fill:	push dword ebx
	push dword edx

	;;  size - multiply by 4 convert to address add to bitmap base
	mov dword eax , [esp + 16] ; size	
	shl dword eax , 2	   ; multiply by 4 ie shift left twice
	mov dword ebx , eax
	
	
	mov dword eax , [esp + 12]
	add dword ebx , eax
		
	;; lea dword ebx , [eax + bitmapsize * 4] 
	
bitmap_fill_loop:	cmp dword eax , ebx
	jae bitmap_fill_done
	
	mov dword [eax] , -1
	add dword eax , 4
	jmp bitmap_fill_loop
	
bitmap_fill_done:
	pop dword edx
	pop dword ebx
	ret
	
;; ;;; clearing bitmap 4 bytes a double word , at a time 32 bits at a time
;; ;;; 
;; bitmap_fill:
;; 	mov dword eax , [esp + 4]
	
;; 	push dword ebx
;; 	push dword edx

;; 	lea dword ebx , [eax + bitmapsize * 4] 
	
;; bitmap_fill_loop:	cmp dword eax , ebx
;; 	jae bitmap_fill_done
	
;; 	mov dword [eax] , -1
;; 	add dword eax , 4
;; 	jmp bitmap_fill_loop
	
;; bitmap_fill_done:
;; 	pop dword edx
;; 	pop dword ebx
;; 	ret


	
	
;;; **********************************************
	section .data	
dummy:	dd 40
	
;;; e.g bitmap 100 bits in length
;;; 32 bits per double word in single register 
bitmapsize:	equ 10
		
	
;;; **********************************************	
	section .bss
;;; on 32 bit system a multiple of 32 for bitmap
bitmap1:		resd  bitmapsize
bitmap2:		resd  bitmapsize
	

	
	


	
