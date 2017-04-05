



;;;  [ PTR RETIP ESI]
global scheme_entry
scheme_entry: nop
	mov dword eax , [ esp + 4 ]

	;; PAiR of (1 . 2)
	mov dword [eax] , 4
	mov dword [eax + 4] , 8

	;; PAiR of (3 . 4)
	mov dword [eax + 8] , 12
	mov dword [eax + 12] , 16

	mov dword eax , [esp + 4]
	add dword eax , 8
	inc dword eax 		; tag as PAIR
	
	ret


	
	

	
	

	

	


