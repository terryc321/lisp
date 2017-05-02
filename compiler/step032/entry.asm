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
;; the integer  5 
MOV DWORD  EAX  , 20
MOV DWORD [ ESP  - 8] ,  EAX 
JMP after150
lambda149: nop
MOV DWORD  EAX  , [ ESP  + 4]
MOV DWORD  EAX  , [ EAX  + 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  + 4]
MOV DWORD  EAX  , [ EAX  + 4]
ADD DWORD  EAX  , [ ESP  - 4]
ret
after150: nop
MOV DWORD  EBX  ,  ESI 
MOV DWORD [ ESI ] , lambda149
ADD DWORD  ESI  , 4
;; local lookup  x 
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESI ] ,  EAX 
ADD DWORD  ESI  , 4
ADD DWORD  EAX  , 8
ADD DWORD  EAX  , -8
MOV DWORD  EAX  ,  EBX 
OR DWORD  EAX  , 6
;; untag closure 
AND DWORD  EAX  , -8
;; save untagged closure on stack 
MOV DWORD [ ESP  - 4] ,  EAX 
;; ;; load CODE address  
MOV DWORD  EAX  , [ EAX ]
;; ;; adjust ESP for non-tail call 
ADD DWORD  ESP  , -4
;; ;; call closure 
CALL  EAX 
;; ;; restore ESP after non-tail call  
SUB DWORD  ESP  , -4
ret
