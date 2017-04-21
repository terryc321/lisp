

global scheme_entry
scheme_entry: mov dword eax , 492
mov dword [ esp -4] , eax 
mov dword eax , [ esp -4] 
mov dword [ esp -8] , eax 
mov dword eax , [ esp -4] 
add dword eax , [ esp -8] 
mov dword [ esp -8] , eax 
mov dword eax , [ esp -8] 
ret


