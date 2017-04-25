
extern debug_stack
global scheme_entry
scheme_entry: nop 
push ebp
mov	ebp, esp
mov dword esi , [ esp + 8 ] 
scheme_heap_in_esi: nop
jmp after144
lambda143: nop
mov dword eax , [ esp -8] 
mov dword [ esp -16] , eax 
mov dword eax , 4 ; integer 1
cmp dword [ esp -16] , eax 
mov dword eax , 0 
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if148
mov dword eax , [ esp -12] 
jmp if149
if148: nop
mov dword eax , [ esp -8] 
mov dword [ esp -24] , eax 
mov dword eax , 4 ; integer 1
sub dword [ esp -24] , eax 
mov dword eax , [ esp -24] 
mov dword [ esp -24] , eax ; save arg 2
mov dword eax , [ esp -8] 
mov dword [ esp -28] , eax 
mov dword eax , [ esp -12] 
shr dword eax , 2 
mov dword ebx , [esp -28]
shr dword ebx , 2 
mul dword ebx
shl dword eax , 2 
mov dword [ esp -28] , eax ; save arg 3
mov dword eax , [ ebp -4] 
mov dword [esp -20] ,eax  ; raw closure ptr 
nop ; collapse arguments by -16
mov dword eax , [ esp +8] ; collapse element 2
mov dword [ esp -24] , eax 
mov dword eax , [ esp +12] ; collapse element 3
mov dword [ esp -28] , eax 
mov dword eax , [esp -20] ; closure ptr 
mov dword [esp -4] , eax ; closure ptr 
mov dword [esp - 4 ] , eax ; closure ptr 
sub dword eax , 110b ; untag closure 
mov dword eax , [eax] ; load procedure ptr from raw closure 
jmp eax ;  tailcall
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
mov dword [esp -4] , eax 
mov dword eax , 20 ; integer 5
mov dword [ esp -16] , eax ; save arg 2
mov dword eax , 4 ; integer 1
mov dword [ esp -20] , eax ; save arg 3
mov dword eax , [ ebp -4] 
sub dword eax , 110b ; untag closure 
mov dword [esp -12 ] , eax ; closure ptr 
mov dword eax , [eax] ; load CODE address 
add dword esp , -4; adjust stack
call eax ; call closure
sub dword esp , -4; restore esp
mov	esp, ebp
pop	ebp
ret
