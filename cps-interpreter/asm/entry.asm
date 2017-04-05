


;;;  [ PTR RETIP ESI]
global scheme_entry
scheme_entry: nop
	mov dword eax , [ esp + 4 ]

	;; PAiR of (1 . 2)
	mov dword [eax] , 4
	mov dword [eax + 4] , 8

	;; PAIR of (3 . 4)
	mov dword [eax + 8] , 12
	mov dword [eax + 12] , 16

	;; PAIR points to
	mov dword esi , [esp + 4]
	mov dword [eax + 16] , esi
	inc dword [eax + 16] 	; tag as PAIR
	
	mov dword [eax + 20] , esi
	add dword [eax + 20] , 8
	inc dword [eax + 20] 	; tag as PAIR

	add dword eax , 16
	inc dword eax 		; tag as PAIR
	
	ret

	
	

	
	

	

	


