extern scheme_cons
extern scheme_closure
extern scheme_make_vector
extern scheme_vector_set
extern scheme_vector_ref
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
MOV DWORD  EAX  , 40
shr dword eax , 2
ADD DWORD  ESP  , -4
PUSH  EAX 
call scheme_make_vector
add dword esp , 4
SUB DWORD  ESP  , -4
mov dword [toplevel + 4] , eax
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
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
mov eax , [toplevel + 4]
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
mov eax , [toplevel + 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 0
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 492
MOV DWORD [ ESP  - 12] ,  EAX 
ADD DWORD  ESP  , -12
call scheme_vector_set
SUB DWORD  ESP  , -12
MOV DWORD  EAX  , 31
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
mov eax , [toplevel + 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 936
MOV DWORD [ ESP  - 12] ,  EAX 
ADD DWORD  ESP  , -12
call scheme_vector_set
SUB DWORD  ESP  , -12
MOV DWORD  EAX  , 31
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
mov eax , [toplevel + 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 8
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 1380
MOV DWORD [ ESP  - 12] ,  EAX 
ADD DWORD  ESP  , -12
call scheme_vector_set
SUB DWORD  ESP  , -12
MOV DWORD  EAX  , 31
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
mov eax , [toplevel + 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 12
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 1824
MOV DWORD [ ESP  - 12] ,  EAX 
ADD DWORD  ESP  , -12
call scheme_vector_set
SUB DWORD  ESP  , -12
MOV DWORD  EAX  , 31
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
mov eax , [toplevel + 4]
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
mov eax , [toplevel + 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 0
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_vector_ref
SUB DWORD  ESP  , -8
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
mov eax , [toplevel + 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_vector_ref
SUB DWORD  ESP  , -8
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
mov eax , [toplevel + 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 36
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_vector_ref
SUB DWORD  ESP  , -8
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
mov eax , [toplevel + 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 8
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_vector_ref
SUB DWORD  ESP  , -8
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
mov eax , [toplevel + 4]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 12
MOV DWORD [ ESP  - 8] ,  EAX 
ADD DWORD  ESP  , -8
call scheme_vector_ref
SUB DWORD  ESP  , -8
PUSH  EAX 
call scheme_pretty_print_nl
add dword esp, 4
ret
