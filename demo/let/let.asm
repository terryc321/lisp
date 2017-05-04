global scheme_entry
global scheme_car
global scheme_cdr
section .data
align 32
toplevel:  times 7 dd 0 
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
JMP after150
lambda149: nop
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 4
CMP DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 0
sete al
SHL DWORD  EAX  , 7
OR DWORD  EAX  , 31
CMP DWORD  EAX  , 31
JE if154
MOV DWORD  EAX  , 4
JMP if155
if154: nop
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 8
CMP DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 0
sete al
SHL DWORD  EAX  , 7
OR DWORD  EAX  , 31
CMP DWORD  EAX  , 31
JE if156
MOV DWORD  EAX  , 4
JMP if157
if156: nop
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
SUB DWORD  EAX  , [ ESP  - 20]
MOV DWORD [ ESP  - 20] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -8
CALL  EAX 
SUB DWORD  ESP  , -8
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 8
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
ADD DWORD  EAX  , [ ESP  - 12]
if157: nop
if155: nop
ret
after150: nop
MOV DWORD  EBX  ,  ESI 
MOV DWORD [ ESI ] , lambda149
ADD DWORD  ESI  , 8
MOV DWORD  EAX  ,  EBX 
OR DWORD  EAX  , 6
mov dword [toplevel + 4] , eax
JMP after159
lambda158: nop
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 8
CMP DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 0
setl al
SHL DWORD  EAX  , 7
OR DWORD  EAX  , 31
CMP DWORD  EAX  , 31
JE if160
MOV DWORD  EAX  , 4
JMP if161
if160: nop
MOV DWORD  EAX  , [ ESP  - 8]
SHR DWORD  EAX  , 2
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 24] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
SUB DWORD  EAX  , [ ESP  - 24]
MOV DWORD [ ESP  - 24] ,  EAX 
mov eax , [toplevel + 8]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -12
CALL  EAX 
SUB DWORD  ESP  , -12
SHR DWORD  EAX  , 2
MUL DWORD [ ESP  - 12]
SHL DWORD  EAX  , 2
if161: nop
ret
after159: nop
MOV DWORD  EBX  ,  ESI 
MOV DWORD [ ESI ] , lambda158
ADD DWORD  ESI  , 8
MOV DWORD  EAX  ,  EBX 
OR DWORD  EAX  , 6
mov dword [toplevel + 8] , eax
JMP after163
lambda162: nop
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 0
CMP DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 0
sete al
SHL DWORD  EAX  , 7
OR DWORD  EAX  , 31
CMP DWORD  EAX  , 31
JE if164
MOV DWORD  EAX  , 47
JMP if165
if164: nop
MOV DWORD  EAX  , 31
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 24] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
SUB DWORD  EAX  , [ ESP  - 24]
MOV DWORD [ ESP  - 24] ,  EAX 
mov eax , [toplevel + 12]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -12
CALL  EAX 
SUB DWORD  ESP  , -12
MOV DWORD [ ESI  + 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 12]
MOV DWORD [ ESI ] ,  EAX 
MOV DWORD  EAX  ,  ESI 
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
ADD DWORD  ESI  , 8
if165: nop
ret
after163: nop
MOV DWORD  EBX  ,  ESI 
MOV DWORD [ ESI ] , lambda162
ADD DWORD  ESI  , 8
MOV DWORD  EAX  ,  EBX 
OR DWORD  EAX  , 6
mov dword [toplevel + 12] , eax
JMP after167
lambda166: nop
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , 8
CMP DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , 0
setl al
SHL DWORD  EAX  , 7
OR DWORD  EAX  , 31
CMP DWORD  EAX  , 31
JE if168
MOV DWORD  EAX  , [ ESP  - 12]
JMP if169
if168: nop
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 24] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
SUB DWORD  EAX  , [ ESP  - 24]
MOV DWORD [ ESP  - 24] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
SHR DWORD  EAX  , 2
MOV DWORD [ ESP  - 28] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 12]
SHR DWORD  EAX  , 2
MUL DWORD [ ESP  - 28]
SHL DWORD  EAX  , 2
MOV DWORD [ ESP  - 28] ,  EAX 
mov eax , [toplevel + 16]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -12
CALL  EAX 
SUB DWORD  ESP  , -12
if169: nop
ret
after167: nop
MOV DWORD  EBX  ,  ESI 
MOV DWORD [ ESI ] , lambda166
ADD DWORD  ESI  , 8
MOV DWORD  EAX  ,  EBX 
OR DWORD  EAX  , 6
mov dword [toplevel + 16] , eax
JMP after171
lambda170: nop
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , 8
CMP DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , 0
setl al
SHL DWORD  EAX  , 7
OR DWORD  EAX  , 31
CMP DWORD  EAX  , 31
JE if172
MOV DWORD  EAX  , [ ESP  - 12]
JMP if173
if172: nop
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
SUB DWORD  EAX  , [ ESP  - 20]
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
SHR DWORD  EAX  , 2
MOV DWORD [ ESP  - 24] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 12]
SHR DWORD  EAX  , 2
MUL DWORD [ ESP  - 24]
SHL DWORD  EAX  , 2
MOV DWORD [ ESP  - 24] ,  EAX 
mov eax , [toplevel + 20]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 24]
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 20]
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 16]
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 4]
MOV DWORD  EAX  , [ EAX ]
JMP  EAX 
if173: nop
ret
after171: nop
MOV DWORD  EBX  ,  ESI 
MOV DWORD [ ESI ] , lambda170
ADD DWORD  ESI  , 8
MOV DWORD  EAX  ,  EBX 
OR DWORD  EAX  , 6
mov dword [toplevel + 20] , eax
MOV DWORD  EAX  , 40
MOV DWORD [ ESP  - 12] ,  EAX 
mov eax , [toplevel + 8]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , 0
CALL  EAX 
SUB DWORD  ESP  , 0
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 36
MOV DWORD [ ESP  - 16] ,  EAX 
mov eax , [toplevel + 8]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -4
CALL  EAX 
SUB DWORD  ESP  , -4
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 20
MOV DWORD [ ESP  - 20] ,  EAX 
mov eax , [toplevel + 8]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -8
CALL  EAX 
SUB DWORD  ESP  , -8
MOV DWORD [ ESI  + 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESI ] ,  EAX 
MOV DWORD  EAX  ,  ESI 
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
ADD DWORD  ESI  , 8
MOV DWORD [ ESI  + 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 4]
MOV DWORD [ ESI ] ,  EAX 
MOV DWORD  EAX  ,  ESI 
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
ADD DWORD  ESI  , 8
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 40
MOV DWORD [ ESP  - 16] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -4
CALL  EAX 
SUB DWORD  ESP  , -4
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 36
MOV DWORD [ ESP  - 20] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -8
CALL  EAX 
SUB DWORD  ESP  , -8
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 20
MOV DWORD [ ESP  - 24] ,  EAX 
mov eax , [toplevel + 4]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -12
CALL  EAX 
SUB DWORD  ESP  , -12
MOV DWORD [ ESI  + 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 12]
MOV DWORD [ ESI ] ,  EAX 
MOV DWORD  EAX  ,  ESI 
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
ADD DWORD  ESI  , 8
MOV DWORD [ ESI  + 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESI ] ,  EAX 
MOV DWORD  EAX  ,  ESI 
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
ADD DWORD  ESI  , 8
MOV DWORD [ ESI  + 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 4]
MOV DWORD [ ESI ] ,  EAX 
MOV DWORD  EAX  ,  ESI 
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
ADD DWORD  ESI  , 8
MOV DWORD  EAX  , 400
MOV DWORD [ ESP  - 12] ,  EAX 
mov eax , [toplevel + 12]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , 0
CALL  EAX 
SUB DWORD  ESP  , 0
MOV DWORD  EAX  , 40
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 16] ,  EAX 
mov eax , [toplevel + 16]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , 0
CALL  EAX 
SUB DWORD  ESP  , 0
MOV DWORD  EAX  , 20
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 16] ,  EAX 
mov eax , [toplevel + 20]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , 0
CALL  EAX 
SUB DWORD  ESP  , 0
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 24
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 20] ,  EAX 
mov eax , [toplevel + 20]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -4
CALL  EAX 
SUB DWORD  ESP  , -4
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , 28
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 24] ,  EAX 
mov eax , [toplevel + 20]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -8
CALL  EAX 
SUB DWORD  ESP  , -8
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 32
MOV DWORD [ ESP  - 24] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 28] ,  EAX 
mov eax , [toplevel + 20]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 20] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -12
CALL  EAX 
SUB DWORD  ESP  , -12
MOV DWORD [ ESI  + 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 12]
MOV DWORD [ ESI ] ,  EAX 
MOV DWORD  EAX  ,  ESI 
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
ADD DWORD  ESI  , 8
MOV DWORD [ ESI  + 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 8]
MOV DWORD [ ESI ] ,  EAX 
MOV DWORD  EAX  ,  ESI 
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
ADD DWORD  ESI  , 8
MOV DWORD [ ESI  + 4] ,  EAX 
MOV DWORD  EAX  , [ ESP  - 4]
MOV DWORD [ ESI ] ,  EAX 
MOV DWORD  EAX  ,  ESI 
AND DWORD  EAX  , -8
OR DWORD  EAX  , 1
ADD DWORD  ESI  , 8
MOV DWORD  EAX  , 28
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 16] ,  EAX 
mov eax , [toplevel + 20]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 8] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , 0
CALL  EAX 
SUB DWORD  ESP  , 0
MOV DWORD [ ESP  - 4] ,  EAX 
MOV DWORD  EAX  , 32
MOV DWORD [ ESP  - 16] ,  EAX 
MOV DWORD  EAX  , 4
MOV DWORD [ ESP  - 20] ,  EAX 
mov eax , [toplevel + 20]
AND DWORD  EAX  , -8
MOV DWORD [ ESP  - 12] ,  EAX 
MOV DWORD  EAX  , [ EAX ]
ADD DWORD  ESP  , -4
CALL  EAX 
SUB DWORD  ESP  , -4
ADD DWORD  EAX  , [ ESP  - 4]
ret
