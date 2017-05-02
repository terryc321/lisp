global scheme_entry
section .data
align 32
toplevel:  times 2 dd 0 
section .text
align 32
;; scheme_entry is called from driver.c 
scheme_entry: nop
;; load heap address into esi , provided by c compiler 
MOV DWORD  ESI  , [ ESP  + 4]
;; the integer  2 
MOV DWORD  EAX  , 8
SHR DWORD  EAX  , 2
MOV DWORD [ ESP  - 8] ,  EAX 
;; the integer  3 
MOV DWORD  EAX  , 12
SHR DWORD  EAX  , 2
MUL DWORD [ ESP  - 8]
SHL DWORD  EAX  , 2
ret
