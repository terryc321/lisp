


;;;     +8  +4   esp
;;;     |   |     |
;;;     |   |    \|/
;;;     .   .     .
;;;  [ PTR RETIP ESI]
global scheme_entry
scheme_entry: nop
	push dword esi 

	mov dword eax , [ esp + 8 ]
	mov dword esi , eax 

	;; PAiR of (1 . 2)
	mov dword [eax] , 4
	mov dword [eax + 4] , 8

	mov dword [eax + 8] , eax
	inc dword [eax + 8]
	mov dword [eax + 12] , 16
	
	
	mov dword eax , esi
	add dword eax , 8
	inc dword eax 		; tag as PAIR

	pop dword esi 
	ret

	

	

	
	

	

	


