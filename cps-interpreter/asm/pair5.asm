


;;;     +8  +4   esp
;;;     |   |     |
;;;     |   |    \|/
;;;     .   .     .
;;;  [ PTR RETIP ESI]
	global scheme_entry
	global scheme_car
	global scheme_cdr
	
scheme_entry: nop

	push dword esi
	
	mov dword eax , [ esp + 8 ]
	mov dword esi , [ esp + 8 ]
	
	
	;; PAiR of (1 . 2)
	;;  A
	mov dword [esi + 0] , 4
	;;  A + 4
	mov dword [esi + 4] , 8
	;; pair of (3 . 4)
	;;  A + 8
	mov dword [esi + 8] , 12
	;;  A + 12
	mov dword [esi + 12] , 16

	;; A + 16
	;; mov dword [esi + 16] , 32
	;; mov dword [eax + 16] , 123
	
	mov dword [esi + 16] , eax
	inc dword [esi + 16]
	
	;; A + 20
	mov dword [esi + 20] , eax
	add dword [esi + 20] , 8
	inc dword [esi + 20]

	;; ;; ;; A + 24 really into EAX reg
	
	mov dword eax , [esp + 8]
	add dword eax , 16
	inc dword eax
	
	pop dword esi
	
	ret
	

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

	
	

	

	
	

	

	


