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
MOV DWORD  EAX  , 400
shr dword eax , 2
ADD DWORD  ESP  , -4
PUSH  EAX 
call scheme_make_vector
add dword esp , 4
SUB DWORD  ESP  , -4
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
MOV DWORD  EAX  , 40
shr dword eax , 2
ADD DWORD  ESP  , -4
PUSH  EAX 
call scheme_make_vector
add dword esp , 4
SUB DWORD  ESP  , -4
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
MOV DWORD  EAX  , 20
shr dword eax , 2
ADD DWORD  ESP  , -4
PUSH  EAX 
call scheme_make_vector
add dword esp , 4
SUB DWORD  ESP  , -4
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
MOV DWORD  EAX  , 4
shr dword eax , 2
ADD DWORD  ESP  , -4
PUSH  EAX 
call scheme_make_vector
add dword esp , 4
SUB DWORD  ESP  , -4
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
ret
