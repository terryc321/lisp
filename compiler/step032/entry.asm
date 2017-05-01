global scheme_entry
section .data
align 32
toplevel:  times 2 dd 0 
section .text
align 32
scheme_entry: nop

MOV DWORD  ESI  , [ ESP  + 4]
JMP after66158
lambda66157: nop

MOV DWORD  EAX  , [ EBP  + 8]
MOV DWORD  ESP  ,  EBP 
