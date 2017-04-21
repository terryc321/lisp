

global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
mov dword eax , -20
and dword eax , 0b0100
shl dword eax , 5
or dword eax , 0b11111
ret



