global scheme_entry
global scheme_car
global scheme_cdr
section .data
align 32
toplevel:  times 2 dd 0 
section .text
align 32
;;  ---------- scheme_car ---------------- 
scheme_car: nop
nop
MOV DWORD  EAX  , [ ESP  + 4]
;; untag CONS  
DEC DWORD  EAX 
;; take the CDR  
MOV DWORD  EAX  , [ EAX ]
ret
;;  ----------- scheme_cdr ---------------- 
scheme_cdr: nop
nop
MOV DWORD  EAX  , [ ESP  + 4]
;; untag CONS  
DEC DWORD  EAX 
;; take the CDR  
MOV DWORD  EAX  , [ EAX  + 4]
ret
align 32
;; scheme_entry is called from driver.c 
scheme_entry: nop
;; load heap address into esi , provided by c compiler 
MOV DWORD  ESI  , [ ESP  + 4]
MOV DWORD  EAX  , 492
;; let binding for variable  g1 
MOV DWORD [ ESP  - 4] ,  EAX 
JMP after150
lambda149: nop
;; closure binding for variable  g1 
MOV DWORD  EAX  , [ ESP  - 4]
MOV DWORD  EAX  , [ EAX  + 8]
MOV DWORD [ ESP  - 8] ,  EAX 
;; closure binding for variable  g1 
MOV DWORD  EAX  , [ ESP  - 4]
MOV DWORD  EAX  , [ EAX  + 8]
ADD DWORD  EAX  , [ ESP  - 8]
ret
after150: nop
MOV DWORD  EBX  ,  ESI 
MOV DWORD [ ESI ] , lambda149
ADD DWORD  ESI  , 8
MOV DWORD  EAX  , [ ESP  - 4]
MOV DWORD [ ESI ] ,  EAX 
ADD DWORD  ESI  , 8
MOV DWORD  EAX  ,  EBX 
OR DWORD  EAX  , 6
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -4
CALL  EAX 
SUB DWORD  ESP  , -4
ret
