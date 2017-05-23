extern allocate
extern last_alloc_esi
extern scheme_cons
extern scheme_closure
global scheme_entry
global scheme_car
global scheme_cdr
section .data
align 32
toplevel:  times 3 dd 0 
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
JMP after160
lambda159: nop
MOV DWORD  EAX  , [ ESP  - 12]
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
CMP DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , 0
setl al
SHL DWORD  EAX  , 7
OR DWORD  EAX  , 31
CMP DWORD  EAX  , 31
JE if161
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 36] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
SUB DWORD  EAX  , [ ESP  - 36]
MOV DWORD [ ESP  - 36] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 12]
MOV DWORD [ ESP  - 40] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 16]
MOV DWORD [ ESP  - 44] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 32] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -24
CALL  EAX 
SUB DWORD  ESP  , -24
MOV DWORD [ ESP  - 28] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 40] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 12]
SUB DWORD  EAX  , [ ESP  - 40]
MOV DWORD [ ESP  - 40] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 16]
MOV DWORD [ ESP  - 44] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESP  - 48] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 36] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -28
CALL  EAX 
SUB DWORD  ESP  , -28
MOV DWORD [ ESP  - 32] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 44] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 16]
SUB DWORD  EAX  , [ ESP  - 44]
MOV DWORD [ ESP  - 44] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESP  - 48] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 12]
MOV DWORD [ ESP  - 52] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 40] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -32
CALL  EAX 
SUB DWORD  ESP  , -32
MOV DWORD [ ESP  - 36] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 24] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -16
CALL  EAX 
SUB DWORD  ESP  , -16
JMP if162
if161: nop
MOV DWORD  EAX  , [ ESP  - 16]
if162: nop
ret
after160: nop
ADD DWORD  ESP  , -4
push dword lambda159
push dword 1
call scheme_closure
add dword esp , 8
SUB DWORD  ESP  , -4
OR DWORD  EAX  , 6
mov dword [toplevel + 4] , eax
MOV DWORD  EAX  , 72
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 48
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , 24
MOV DWORD [ ESP  - 20] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , 0
CALL  EAX 
SUB DWORD  ESP  , 0
ret
