extern allocate
extern last_alloc_esi
extern scheme_cons
extern scheme_closure
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
MOV DWORD  EAX  , 20
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 28
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_cons
SUB DWORD  ESP  , -8
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 8
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_cons
SUB DWORD  ESP  , -8
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 12
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 16
MOV DWORD [ ESP  - 12] ,  EAX 
ADD DWORD  ESP  , -12
call scheme_cons
SUB DWORD  ESP  , -12
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_cons
SUB DWORD  ESP  , -8
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 8
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 12
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 16
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , 20
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , 24
MOV DWORD [ ESP  - 24] ,  EAX 
MOV DWORD  EAX  , 28
MOV DWORD [ ESP  - 28] ,  EAX 
MOV DWORD  EAX  , 32
MOV DWORD [ ESP  - 32] ,  EAX 
MOV DWORD  EAX  , 36
MOV DWORD [ ESP  - 36] ,  EAX 
MOV DWORD  EAX  , 40
MOV DWORD [ ESP  - 40] ,  EAX 
MOV DWORD  EAX  , 47
MOV DWORD [ ESP  - 44] ,  EAX 
ADD DWORD  ESP  , -44
call scheme_cons
SUB DWORD  ESP  , -44
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 40] ,  EAX 
ADD DWORD  ESP  , -40
call scheme_cons
SUB DWORD  ESP  , -40
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 36] ,  EAX 
ADD DWORD  ESP  , -36
call scheme_cons
SUB DWORD  ESP  , -36
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 32] ,  EAX 
ADD DWORD  ESP  , -32
call scheme_cons
SUB DWORD  ESP  , -32
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 28] ,  EAX 
ADD DWORD  ESP  , -28
call scheme_cons
SUB DWORD  ESP  , -28
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 24] ,  EAX 
ADD DWORD  ESP  , -24
call scheme_cons
SUB DWORD  ESP  , -24
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 20] ,  EAX 
ADD DWORD  ESP  , -20
call scheme_cons
SUB DWORD  ESP  , -20
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 16] ,  EAX 
ADD DWORD  ESP  , -16
call scheme_cons
SUB DWORD  ESP  , -16
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 12] ,  EAX 
ADD DWORD  ESP  , -12
call scheme_cons
SUB DWORD  ESP  , -12
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_cons
SUB DWORD  ESP  , -8
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 8
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 12
MOV DWORD [ ESP  - 12] ,  EAX 
ADD DWORD  ESP  , -12
call scheme_cons
SUB DWORD  ESP  , -12
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_cons
SUB DWORD  ESP  , -8
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 16
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 20
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 24
MOV DWORD [ ESP  - 16] ,  EAX 
ADD DWORD  ESP  , -16
call scheme_cons
SUB DWORD  ESP  , -16
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 12] ,  EAX 
ADD DWORD  ESP  , -12
call scheme_cons
SUB DWORD  ESP  , -12
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 28
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 32
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , 36
MOV DWORD [ ESP  - 20] ,  EAX 
ADD DWORD  ESP  , -20
call scheme_cons
SUB DWORD  ESP  , -20
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 16] ,  EAX 
ADD DWORD  ESP  , -16
call scheme_cons
SUB DWORD  ESP  , -16
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 12] ,  EAX 
ADD DWORD  ESP  , -12
call scheme_cons
SUB DWORD  ESP  , -12
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_cons
SUB DWORD  ESP  , -8
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
ret
