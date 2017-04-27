
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
jmp after3115
lambda3114: nop ; 
mov dword eax , [ esp -8] 
mov dword [ esp -12] , eax 
mov dword eax , 20 ; integer 5
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -12] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
ret
after3115: nop
mov dword ebx , esi
mov dword [ esi ] , lambda3114
add dword esi , 4 
add dword eax , 8
and dword eax , (-8) 
mov dword eax , ebx
or dword eax , 110b 
mov dword ebx , toplevel ;; toplevel define f4
mov dword [ebx + 4] , eax
mov dword eax , 20 ; integer 5
mov dword [esp -12] , eax
mov dword ebx , toplevel ;; toplevel define f4
mov dword eax , [ebx + 4]
and dword eax , -8 ; untag closure -8 is binary 11...1111000  lower 3 bits zero
mov dword [esp -8 ] , eax ; closure ptr 
mov dword eax , [eax] ; load CODE address 
add dword esp , 0; adjust stack
call eax ; call closure
sub dword esp , 0; restore esp
ret
