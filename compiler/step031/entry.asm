
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
mov dword esi , [ ebp + 8 ] 
scheme_heap_in_esi: nop
jmp after144
lambda143: nop ; 
mov dword eax , [ esp -8] 
mov dword [ esp -12] , eax 
mov dword eax , 4 ; integer 1
cmp dword [ esp -12] , eax 
mov dword eax , 0 
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if148
mov dword eax , 4 ; integer 1
jmp if149
if148: nop
mov dword eax , [ esp -8] 
mov dword [ esp -12] , eax 
mov dword eax , [ esp -8] 
mov dword [ esp -24] , eax 
mov dword eax , 4 ; integer 1
sub dword [ esp -24] , eax
mov dword eax , [ esp -24] 
push dword eax
mov dword ebx , toplevel ;; toplevel define fac
mov dword eax , [ebx + 4]
and dword eax , -8 ; untag closure -8 is binary 11...1111000  lower 3 bits zero
mov dword [esp -20 ] , eax ; closure ptr 
mov dword eax , [eax] ; load CODE address 
add dword esp , -12; adjust stack
call eax ; call closure
sub dword esp , -12; restore esp
shr dword eax , 2 
mov dword ebx , [esp -12]
shr dword ebx , 2 
mul dword ebx
shl dword eax , 2 
if149: nop
ret
after144: nop
mov dword ebx , esi
mov dword [ esi ] , lambda143
add dword esi , 4 
add dword eax , 8
and dword eax , (-8) 
mov dword eax , ebx
or dword eax , 110b 
mov dword ebx , toplevel ;; toplevel define fac
mov dword [ebx + 4] , eax
mov dword eax , 20 ; integer 5
push dword eax
mov dword ebx , toplevel ;; toplevel define fac
mov dword eax , [ebx + 4]
and dword eax , -8 ; untag closure -8 is binary 11...1111000  lower 3 bits zero
mov dword [esp -8 ] , eax ; closure ptr 
mov dword eax , [eax] ; load CODE address 
add dword esp , 0; adjust stack
call eax ; call closure
sub dword esp , 0; restore esp
mov	esp, ebp
pop	ebp
ret
