
;;; **********************************************
	section .data	
dummy:	dd 40
	
;;; e.g bitmap 100 bits in length
;;; 32 bits per double word in single register
heapsize:	equ (32 * 10)
bitmapsize:	equ (heapsize / 32)

	
;;; **********************************************	
	section .bss
;;; on 32 bit system a multiple of 32 for bitmap
heap1:		resd heapsize
heap2:		resd heapsize	
headerBitmap1:		resd  bitmapsize
headerBitmap2:		resd  bitmapsize
forwardBitmap1:		resd  bitmapsize
forwardBitmap2:		resd  bitmapsize

	

;;; **********************************************	
	global alloc
	
;;; **********************************************
	section .text

;;; [ C stack ]
;;; n : : number cells to allocate
;;; ret-ip : : the return address
alloc:	mov dword eax , [esp + 4] ; n 
	shl dword eax , 2 	; multiply by 4 : shift left twice
	ret

	
	
	
	

	
	


	
