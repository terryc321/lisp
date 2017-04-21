
extern debug_stack
global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
scheme_heap_in_esi: nop
jmp after9781
fac: nop
mov dword eax , [ esp -4] 
mov dword [ esp -8] , eax 
mov dword eax , 4
cmp dword [ esp -8] , eax 
mov dword eax , 0 
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if9782
mov dword eax , 4
jmp if9783
if9782: nop
mov dword eax , [ esp -4] 
mov dword [ esp -8] , eax 
mov dword eax , [ esp -4] 
mov dword [ esp -16] , eax 
mov dword eax , 4
sub dword [ esp -16] , eax 
mov dword eax , [ esp -16] 
mov dword [ esp -16] , eax ; save arg 1
add dword esp , -8; adjust esp
call fac
sub dword esp , -8; restore esp
shr dword eax , 2 
mov dword ebx , [esp -8]
shr dword ebx , 2 
mul dword ebx
shl dword eax , 2 
if9783: nop
ret
after9781: nop
mov dword eax , 40
mov dword [ esp -8] , eax ; save arg 1
add dword esp , 0; adjust esp
call fac
sub dword esp , 0; restore esp
ret
