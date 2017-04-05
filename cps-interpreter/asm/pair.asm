



;;;  [ PTR RETIP ESI]
global scheme_entry
scheme_entry: nop
	mov dword eax , [ esp + 4 ]

	;; PAiR of (1 . 2)
	mov dword [eax] , 4
	mov dword [eax + 4] , 8
	
	inc dword eax 		; tag as PAIR
	
	ret

	
	

	
	

	

	


