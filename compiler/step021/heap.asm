
	

global scheme_entry
scheme_entry: nop 
	mov dword esi , [ esp + 4 ]

	;; (#t . #f)
	mov dword [esi] , 159	
	mov dword [esi + 4] , 31
	mov dword eax , esi
	inc dword eax
	mov dword [esi + 16] , eax
	
	;; (#t . #f)
	mov dword [esi + 8] , 159	
	mov dword [esi + 12] , 31
	mov dword eax , esi
	add dword eax , 9
	mov dword [esi + 20] , eax
	
	mov dword eax , esi
	add dword eax , 16
	;; tag PAIR
	inc dword eax
	
	;; mov dword eax , esi
	;; mov dword [esi + 16] , eax

	;; mov dword eax , esi
	;; add dword eax , 8
	;; mov dword [esi + 20] , eax

	;; mov dword eax , esi
	;; add dword eax , 16

	;; bump
add dword esi , 24
	ret

	
	


	
	


