extern scheme_cons
extern scheme_closure
extern scheme_make_vector
extern scheme_pretty_print
extern scheme_pretty_print_nl
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
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 0
CMP DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 0
sete al
SHL DWORD  EAX  , 7
OR DWORD  EAX  , 31
CMP DWORD  EAX  , 31
JE if161
MOV DWORD  EAX  , 47
JMP if162
if161: nop
MOV DWORD  EAX  , 31
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 24] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
SUB DWORD  EAX  , [ ESP  - 24]
MOV DWORD [ ESP  - 24] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -12
CALL  EAX 
SUB DWORD  ESP  , -12
MOV DWORD [ ESP  - 16] ,  EAX 
ADD DWORD  ESP  , -16
call scheme_cons
SUB DWORD  ESP  , -16
if162: nop
ret
after160: nop
ADD DWORD  ESP  , -4
push dword lambda159
push dword 1
call scheme_closure
add dword esp , 8
SUB DWORD  ESP  , -4
mov dword [toplevel + 4] , eax
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
MOV DWORD  EAX  , 400
MOV DWORD [ ESP  - 12] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , 0
CALL  EAX 
SUB DWORD  ESP  , 0
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
ret
