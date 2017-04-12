
extern debug_stack
global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
scheme_heap_in_esi: nop
mov dword eax , 72
mov dword [ esp -8] , eax ; save X 
mov dword eax , 48
mov dword [ esp -12] , eax ; save Y
mov dword eax , 24
mov dword [ esp -16] , eax ; save Z
add dword esp , 0; adjust esp
call tak ; -16
sub dword esp , 0; restore esp
ret



