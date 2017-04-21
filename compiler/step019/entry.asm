

global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
mov dword eax , -20
mov dword [ esp -4] , eax 
mov dword eax , -12
cmp dword [ esp -4] , eax 
mov dword eax , 0 
setge al
shl dword eax , 7
or dword eax , 31
ret



