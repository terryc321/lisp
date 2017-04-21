

global scheme_entry
fib: nop
mov dword eax , [ esp -4] 
mov dword [ esp -8] , eax 
mov dword eax , 4
cmp dword [ esp -8] , eax 
mov dword eax , 0 
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if49
mov dword eax , 4
jmp if50
if49: nop
mov dword eax , [ esp -4] 
mov dword [ esp -8] , eax 
mov dword eax , 8
cmp dword [ esp -8] , eax 
mov dword eax , 0 
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if52
mov dword eax , 4
jmp if53
if52: nop
mov dword eax , [ esp -4] 
mov dword [ esp -12] , eax 
mov dword eax , 4
sub dword [ esp -12] , eax 
mov dword eax , [ esp -12] 
mov dword [ esp -12] , eax 
add dword esp , -4
call fib
sub dword esp , -4
mov dword [ esp -8] , eax 
mov dword eax , [ esp -4] 
mov dword [ esp -16] , eax 
mov dword eax , 8
sub dword [ esp -16] , eax 
mov dword eax , [ esp -16] 
mov dword [ esp -16] , eax 
add dword esp , -8
call fib
sub dword esp , -8
add dword eax , [ esp -8] 
if53: nop
if50: nop
ret
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
je if54
mov dword eax , 4
jmp if55
if54: nop
mov dword eax , [ esp -4] 
mov dword [ esp -8] , eax 
mov dword eax , [ esp -4] 
mov dword [ esp -16] , eax 
mov dword eax , 4
sub dword [ esp -16] , eax 
mov dword eax , [ esp -16] 
mov dword [ esp -16] , eax 
add dword esp , -8
call fac
sub dword esp , -8
shr dword eax , 2 
mov dword ebx , [esp -8]
shr dword ebx , 2 
mul dword ebx
shl dword eax , 2 
if55: nop
ret
f3x: nop
mov dword eax , [ esp -4] 
ret
f3y: nop
mov dword eax , [ esp -8] 
ret
f3z: nop
mov dword eax , [ esp -12] 
ret
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
mov dword eax , 4
mov dword [ esp -12] , eax 
mov dword eax , 8
mov dword [ esp -16] , eax 
mov dword eax , 12
mov dword [ esp -20] , eax 
add dword esp , -4
call f3z
add dword esp , 4
mov dword [ esp -8] , eax 
mov dword eax , 16
mov dword [ esp -12] , eax 
mov dword eax , 20
mov dword [ esp -16] , eax 
mov dword eax , [ esp -16] 
mov dword [ esp -16] , eax 
add dword esp , 0
call f3x
add dword esp , 0
ret



