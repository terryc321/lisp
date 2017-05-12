global scheme_entry
section .data
align 32
toplevel:  times 3 dd 0 
section .text
align 32
;; scheme_entry is called from driver.c 
scheme_entry: nop
;; load heap address into esi , provided by c compiler 
MOV DWORD  ESI  , [ ESP  + 4]
JMP after150
lambda149: nop
MOV DWORD  EAX  , [ ESP  - 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 4
CMP DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 0
setl al
SHL DWORD  EAX  , 7
OR DWORD  EAX  , 31
CMP DWORD  EAX  , 31
JE if154
MOV DWORD  EAX  , 4
JMP if155
if154: nop
MOV DWORD  EAX  , [ ESP  - 4]
SHR DWORD  EAX  , 2
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 4]
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 4
SUB DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 12]
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EBX  , toplevel
MOV DWORD  EAX  , [ EBX  + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -4
CALL  EAX 
SUB DWORD  ESP  , -4
SHR DWORD  EAX  , 2
MUL DWORD [ ESP  - 4]
SHL DWORD  EAX  , 2
if155: nop
ret
after150: nop
MOV DWORD  EBX  ,  ESI 
MOV DWORD [ ESI ] , lambda149
ADD DWORD  ESI  , 4
ADD DWORD  EAX  , 8
AND DWORD  EAX  , -8
MOV DWORD  EAX  ,  EBX 
OR DWORD  EAX  , 6
mov dword ebx , toplevel
mov dword [ebx + 4] , eax
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EBX  , toplevel
MOV DWORD  EAX  , [ EBX  + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP ] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , 0
CALL  EAX 
SUB DWORD  ESP  , 0
ret
