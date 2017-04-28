
extern debug_stack
global scheme_entry
section .data
align 32
toplevel: times 5 dd 0
section .text
align 32
scheme_entry: nop 
push ebp
mov	ebp, esp
mov dword esi , [ esp + 8 ] 
scheme_heap_in_esi: nop
jmp after144
lambda143: nop ; 
mov dword eax , 12 ; integer 3
mov dword [ esi ] , eax 
mov dword [ esi + 4 ] , 31
mov dword ebx , eax 
shr dword ebx , 2
add dword ebx , 4
mov dword eax , esi 
or dword eax , 2 
mov dword ecx , eax
add dword esi , 8
bump148: mov dword [ esi ] , 31
add dword esi , 4
dec dword ebx 
cmp dword ebx , 0 
ja bump148
add dword eax , 8
and dword eax , (-8) 
mov dword eax, ecx
ret
after144: nop
mov dword ebx , esi
mov dword [ esi ] , lambda143
add dword esi , 4 
add dword eax , 8
and dword eax , (-8) 
mov dword eax , ebx
or dword eax , 110b 
mov dword ebx , toplevel ;; toplevel define f
mov dword [ebx + 4] , eax
mov dword eax , 4 ; integer 1
mov dword eax , 8 ; integer 2
mov dword eax , 12 ; integer 3
mov dword ebx , toplevel ;; toplevel define f
mov dword eax , [ebx + 4]
and dword eax , -8 ; untag closure 
mov dword [esp -12 ] , eax ; closure ptr 
mov dword eax , [eax] ; load CODE address 
add dword esp , -4; adjust stack
call eax ; call closure
sub dword esp , -4; restore esp
mov	esp, ebp
pop	ebp
ret
