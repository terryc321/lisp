
extern debug_stack
global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
scheme_heap_in_esi: nop
mov dword eax , 40
mov dword [ esp -4] , eax 
mov dword eax , -40
sub dword [ esp -4] , eax 
mov dword eax , [ esp -4] 
ret



