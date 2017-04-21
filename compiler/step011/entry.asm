

global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
mov dword eax , 159
mov dword [ esp -4] , eax 
mov dword eax , 31
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -4] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
ret


