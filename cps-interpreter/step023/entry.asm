
extern debug_stack
global scheme_entry
tak: nop
mov dword eax , [ esp -8] 
mov dword [ esp -16] , eax 
mov dword eax , [ esp -4] 
cmp dword [ esp -16] , eax 
mov dword eax , 0 
setl al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if49
nop ; TAK call now 
nop ; TAK call now 
mov dword eax , [ esp -4] 
mov dword [ esp -24] , eax 
mov dword eax , 4
sub dword [ esp -24] , eax 
mov dword eax , [ esp -24] 
mov dword [ esp -24] , eax 
mov dword eax , [ esp -8] 
mov dword [ esp -28] , eax 
mov dword eax , [ esp -12] 
mov dword [ esp -32] , eax 
add dword esp , -16
call tak
sub dword esp , -16
mov dword [ esp -20] , eax 
nop ; TAK call now 
mov dword eax , [ esp -8] 
mov dword [ esp -28] , eax 
mov dword eax , 4
sub dword [ esp -28] , eax 
mov dword eax , [ esp -28] 
mov dword [ esp -28] , eax 
mov dword eax , [ esp -12] 
mov dword [ esp -32] , eax 
mov dword eax , [ esp -4] 
mov dword [ esp -36] , eax 
add dword esp , -20
call tak
sub dword esp , -20
mov dword [ esp -24] , eax 
nop ; TAK call now 
mov dword eax , [ esp -12] 
mov dword [ esp -32] , eax 
mov dword eax , 4
sub dword [ esp -32] , eax 
mov dword eax , [ esp -32] 
mov dword [ esp -32] , eax 
mov dword eax , [ esp -4] 
mov dword [ esp -36] , eax 
mov dword eax , [ esp -8] 
mov dword [ esp -40] , eax 
add dword esp , -24
call tak
sub dword esp , -24
mov dword [ esp -28] , eax 
add dword esp , -12
call tak
sub dword esp , -12
jmp if50
if49: nop
mov dword eax , [ esp -12] 
if50: nop
ret
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
je if52
mov dword eax , 4
jmp if53
if52: nop
mov dword eax , [ esp -4] 
mov dword [ esp -8] , eax 
mov dword eax , 8
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
if55: nop
if53: nop
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
je if56
mov dword eax , 4
jmp if57
if56: nop
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
if57: nop
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
nop ; TAK call now 
mov dword eax , 72
mov dword [ esp -8] , eax 
mov dword eax , 48
mov dword [ esp -12] , eax 
mov dword eax , 24
mov dword [ esp -16] , eax 
add dword esp , 0
call tak
sub dword esp , 0
ret



