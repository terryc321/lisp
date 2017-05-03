
extern debug_stack
global scheme_entry
section .data
align 32
toplevel: times 5 dd 0
section .text
align 32
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
scheme_heap_in_esi: nop
jmp after144
lambda143: nop ; 
mov dword eax , 4 ; integer 1
mov dword [ esp -8] , eax 
mov dword eax , 8 ; integer 2
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -8] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
ret
after144: nop
mov dword ebx , esi
mov dword [ esi ] , lambda143
add dword esi , 4 
add dword eax , 8
and dword eax , (-8) 
mov dword eax , ebx
or dword eax , 110b 
mov dword ebx , toplevel ;; toplevel define f7
mov dword [ebx + 4] , eax
mov dword ebx , toplevel ;; toplevel define f7
mov dword eax , [ebx + 4]
and dword eax , -8 ; untag closure -8 is binary 11...1111000  lower 3 bits zero
mov dword [esp -8 ] , eax ; closure ptr 
mov dword eax , [eax] ; load CODE address 
add dword esp , -4; adjust stack
call eax ; call closure
sub dword esp , -4; restore esp
ret
