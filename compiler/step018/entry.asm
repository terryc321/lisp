

global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
mov dword eax , -20
mov dword [ esp -4] , eax 
mov dword eax , -12
cmp dword [ esp -4] , eax 
jl true49
mov dword eax , 31
jmp done48
true49: mov dword eax , 159
done48: nop
ret



