


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
		
	;; PAiR of (3 . 4)
	mov dword [eax + 8] , 12
	mov dword [eax + 12] , 16

	mov dword esi , [esp + 8 ]

	;; PAiR of ((1 . 2) . (3 . 4))
	mov dword [eax + 16] , esi
	inc dword [eax + 16] 	; ptr TAG
	
	mov dword [eax + 20] , esi
	add dword [eax + 20] , 8
	inc dword [eax + 20] 	; ptr TAG
	
	;; Pair ptr to eax
	;; Pair ptr to eax + 8
	;; Pair ptr to this object also
	mov dword eax , esi
	add dword eax , 16
	inc dword eax 		; tag as PAIR

	pop dword esi 
	ret

	
	

	
	

	

	


