
extern debug_stack
global scheme_entry
section .data
align 32
toplevel: times 4 dd 0
section .text
align 32
scheme_entry: nop 
push ebp
mov	ebp, esp
mov dword esi , [ ebp + 8 ] 
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
mov dword eax , 12 ; integer 3
mov dword [ esp -4] , eax 
mov dword eax , 16 ; integer 4
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -4] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
mov dword eax , 20 ; integer 5
mov dword [ esp -4] , eax 
mov dword eax , 24 ; integer 6
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -4] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
mov dword eax , 28 ; integer 7
mov dword [ esp -4] , eax 
mov dword eax , 32 ; integer 8
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -4] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
mov dword eax , 36 ; integer 9
mov dword [ esp -4] , eax 
mov dword eax , 40 ; integer 10
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -4] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
mov	esp, ebp
pop	ebp
ret
