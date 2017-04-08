

global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
mov dword eax , 28
mov dword [ esp -4] , eax 
mov dword eax , 32
add dword eax , [ esp -4] 
mov dword [ esi ] , eax 
mov dword [ esi + 4 ] , 31
mov dword ebx , eax 
shr dword ebx , 2
add dword ebx , 4
mov dword eax , esi 
add dword eax , 2 
push dword eax
add dword esi , 8
bump48: mov dword [ esi ] , 31
mov dword [ esi + 4 ] , 31
add dword esi , 8
dec dword ebx 
dec dword ebx 
cmp dword ebx , 2 
ja bump48
pop dword eax
ret



