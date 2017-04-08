
extern debug_stack
global scheme_entry

tak1: nop
      jmp tak

tak2: nop
      jmp tak

tak3: nop
      jmp tak

tak4: nop
      jmp tak

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
mov dword eax , [ esp -4] 
mov dword [ esp -24] , eax 
mov dword eax , 4
sub dword [ esp -24] , eax 
mov dword eax , [ esp -24] 
mov dword [ esp -24] , eax ; save X 
mov dword eax , [ esp -8] 
mov dword [ esp -28] , eax ; save Y
mov dword eax , [ esp -12] 
mov dword [ esp -32] , eax ; save Z
add dword esp , -16; adjust esp
call tak1 ; -32
sub dword esp , -16; restore esp
mov dword [ esp -20] , eax ; save X 
mov dword eax , [ esp -8] 
mov dword [ esp -28] , eax 
mov dword eax , 4
sub dword [ esp -28] , eax 
mov dword eax , [ esp -28] 
mov dword [ esp -28] , eax ; save X 
mov dword eax , [ esp -12] 
mov dword [ esp -32] , eax ; save Y
mov dword eax , [ esp -4] 
mov dword [ esp -36] , eax ; save Z
add dword esp , -20; adjust esp
call tak2 ; -36
sub dword esp , -20; restore esp
mov dword [ esp -24] , eax ; save Y
mov dword eax , [ esp -12] 
mov dword [ esp -32] , eax 
mov dword eax , 4
sub dword [ esp -32] , eax 
mov dword eax , [ esp -32] 
mov dword [ esp -32] , eax ; save X 
mov dword eax , [ esp -4] 
mov dword [ esp -36] , eax ; save Y
mov dword eax , [ esp -8] 
mov dword [ esp -40] , eax ; save Z
add dword esp , -24; adjust esp
call tak3 ; -40
sub dword esp , -24; restore esp
mov dword [ esp -28] , eax ; save Z
add dword esp , -12; adjust esp
call tak4 ; -28
sub dword esp , -12; restore esp
jmp if50
if49: nop
mov dword eax , [ esp -12] 
if50: nop
ret
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
scheme_heap_in_esi: nop
mov dword eax , 72
mov dword [ esp -8] , eax ; save X 
mov dword eax , 48
mov dword [ esp -12] , eax ; save Y
mov dword eax , 24
mov dword [ esp -16] , eax ; save Z
add dword esp , 0; adjust esp
call tak ; -16
sub dword esp , 0; restore esp
ret



