
extern debug_stack
global scheme_entry
section .data
align 32
toplevel: times 4 dd 0
section .text
align 32
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
scheme_heap_in_esi: nop
mov dword eax , 4 ; integer 1
mov dword [ esp -4] , eax 
mov dword eax , 8 ; integer 2
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -4] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
ret
