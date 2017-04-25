
extern debug_stack
global scheme_entry
scheme_entry: nop 
push ebp
mov	ebp, esp
mov dword esi , [ esp + 8 ] 
scheme_heap_in_esi: nop
mov dword eax , 16 ; integer 4
mov dword [ esp -4] , eax 
mov dword eax , 20 ; integer 5
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -4] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
mov	esp, ebp
pop	ebp
ret
