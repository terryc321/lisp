

global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
mov dword eax , 16
mov dword ebx , 3
mul dword ebx
add dword eax , 4
ret



