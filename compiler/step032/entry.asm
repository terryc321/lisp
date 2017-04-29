
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
mov dword esi , [ esp + 8 ] 
scheme_heap_in_esi: nop
mov dword eax , 20 ; integer 5
push dword eax
mov dword eax , [ ebp + -8] 
push dword eax
mov dword eax , [ ebp + -8] 
add dword eax , [ esp ]
add dword esp , 4
mov	esp, ebp
pop	ebp
ret
