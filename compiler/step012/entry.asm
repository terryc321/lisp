

global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
mov dword eax , 4
mov dword [ esp -4] , eax 
mov dword eax , 8
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -4] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
mov dword [ esp -4] , eax 
mov dword eax , 12
mov dword [ esp -8] , eax 
mov dword eax , 16
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -8] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -4] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
mov dword eax , [ eax + 3 ] 
ret


