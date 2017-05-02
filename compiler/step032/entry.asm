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
MOV DWORD  EAX  , 159
CMP DWORD  EAX  , 