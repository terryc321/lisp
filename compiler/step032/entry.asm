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
;; the integer  10 
MOV DWORD  EAX  , 40
;; app help : arg  
MOV DWORD [ ESP ] ,  EAX 
JMP after11475
lambda11474: nop
;; local lookup  x 
MOV DWORD  EAX  , [ ESP  - 4]
ret
after11475: nop
MOV DWORD  EBX  ,  ESI 
MOV DWORD [ ESI ] , lambda11474
ADD DWORD  ESI  , 4
ADD DWORD  EAX  , 8
ADD DWORD  EAX  , -8
MOV DWORD  EAX  ,  EBX 
OR DWORD  EAX  , 6
;; untag closure 
AND DWORD  EAX  , -8
;; save untagged closure on stack 
MOV DWORD [ ESP ] ,  EAX 
;; ;; load CODE address  
MOV DWORD  EAX  , [ EAX ]
;; ;; call closure 
CALL  EAX 
ret
