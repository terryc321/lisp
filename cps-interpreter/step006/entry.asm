

global scheme_entry
scheme_entry: mov dword eax , 47
and dword eax , 3
cmp dword eax , 0
mov dword eax , 0
sete al
shl dword eax , 7
or dword eax , 31
ret


