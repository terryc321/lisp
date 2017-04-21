
extern debug_stack
global scheme_entry
scheme_entry: nop 
mov dword esi , [ esp + 4 ] 
scheme_heap_in_esi: nop
jmp after143
fn_seq: nop
mov dword eax , [ esp -4] 
mov dword [ esp -8] , eax 
mov dword eax , 0
cmp dword [ esp -8] , eax 
mov dword eax , 0 
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if148
mov dword eax , 47
jmp if149
if148: nop
mov dword eax , 31
mov dword [ esp -8] , eax 
mov dword eax , [ esp -4] 
mov dword [ esp -16] , eax 
mov dword eax , 4
sub dword [ esp -16] , eax 
mov dword eax , [ esp -16] 
mov dword [ esp -16] , eax ; save arg 1
add dword esp , -8; adjust esp
call fn_seq
sub dword esp , -8; restore esp
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -8] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
if149: nop
ret
after143: nop
jmp after150
fn_toggle: nop
mov dword eax , [ esp -4] 
cmp dword eax , 31
mov dword eax , 0
sete al
shl dword eax , 7
or dword eax , 31
ret
after150: nop
jmp after151
fn_toggle_every_helper: nop
mov dword eax , [ esp -12] 
cmp dword eax , 47
mov dword eax , 0
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if152
mov dword eax , 47
jmp if153
if152: nop
mov dword eax , [ esp -4] 
mov dword [ esp -16] , eax 
mov dword eax , 4
cmp dword [ esp -16] , eax 
mov dword eax , 0 
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if154
mov dword eax , [ esp -12] 
mov dword eax , [ eax - 1 ] 
mov dword [ esp -20] , eax ; save arg 1
add dword esp , -12; adjust esp
call fn_toggle
sub dword esp , -12; restore esp
mov dword [ esp -16] , eax 
mov dword eax , [ esp -8] 
mov dword [ esp -24] , eax ; save arg 1
mov dword eax , [ esp -8] 
mov dword [ esp -28] , eax ; save arg 2
mov dword eax , [ esp -12] 
mov dword eax , [ eax + 3 ] 
mov dword [ esp -32] , eax ; save arg 3
add dword esp , -16; adjust esp
call fn_toggle_every_helper
sub dword esp , -16; restore esp
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -16] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
jmp if155
if154: nop
mov dword eax , [ esp -12] 
mov dword eax , [ eax - 1 ] 
mov dword [ esp -16] , eax 
mov dword eax , [ esp -4] 
mov dword [ esp -24] , eax 
mov dword eax , 4
sub dword [ esp -24] , eax 
mov dword eax , [ esp -24] 
mov dword [ esp -24] , eax ; save arg 1
mov dword eax , [ esp -8] 
mov dword [ esp -28] , eax ; save arg 2
mov dword eax , [ esp -12] 
mov dword eax , [ eax + 3 ] 
mov dword [ esp -32] , eax ; save arg 3
add dword esp , -16; adjust esp
call fn_toggle_every_helper
sub dword esp , -16; restore esp
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -16] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
if155: nop
if153: nop
ret
after151: nop
jmp after156
fn_toggle_every: nop
mov dword eax , [ esp -4] 
mov dword [ esp -16] , eax ; save arg 1
mov dword eax , [ esp -4] 
mov dword [ esp -20] , eax ; save arg 2
mov dword eax , [ esp -8] 
mov dword [ esp -24] , eax ; save arg 3
add dword esp , -8; adjust esp
call fn_toggle_every_helper
sub dword esp , -8; restore esp
ret
after156: nop
jmp after157
fn_toggle_nth: nop
mov dword eax , [ esp -4] 
mov dword [ esp -12] , eax 
mov dword eax , 400
cmp dword [ esp -12] , eax 
mov dword eax , 0 
setg al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if158
mov dword eax , [ esp -8] 
jmp if159
if158: nop
mov dword eax , [ esp -4] 
mov dword [ esp -16] , eax 
mov dword eax , 4
add dword eax , [ esp -16] 
mov dword [ esp -16] , eax ; save arg 1
mov dword eax , [ esp -4] 
mov dword [ esp -24] , eax ; save arg 1
mov dword eax , [ esp -8] 
mov dword [ esp -28] , eax ; save arg 2
add dword esp , -16; adjust esp
call fn_toggle_every
sub dword esp , -16; restore esp
mov dword [ esp -20] , eax ; save arg 2
add dword esp , -8; adjust esp
call fn_toggle_nth
sub dword esp , -8; restore esp
if159: nop
ret
after157: nop
jmp after160
fn_tog_to_n: nop
mov dword eax , [ esp -8] 
cmp dword eax , 47
mov dword eax , 0
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if161
mov dword eax , 47
jmp if162
if161: nop
mov dword eax , [ esp -8] 
mov dword eax , [ eax - 1 ] 
cmp dword eax , 31
je if163
mov dword eax , [ esp -4] 
mov dword [ esp -12] , eax 
mov dword eax , [ esp -4] 
mov dword [ esp -20] , eax 
mov dword eax , 4
add dword eax , [ esp -20] 
mov dword [ esp -20] , eax ; save arg 1
mov dword eax , [ esp -8] 
mov dword eax , [ eax + 3 ] 
mov dword [ esp -24] , eax ; save arg 2
add dword esp , -12; adjust esp
call fn_tog_to_n
sub dword esp , -12; restore esp
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -12] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
jmp if164
if163: nop
mov dword eax , [ esp -4] 
mov dword [ esp -16] , eax 
mov dword eax , 4
add dword eax , [ esp -16] 
mov dword [ esp -16] , eax ; save arg 1
mov dword eax , [ esp -8] 
mov dword eax , [ eax + 3 ] 
mov dword [ esp -20] , eax ; save arg 2
add dword esp , -8; adjust esp
call fn_tog_to_n
sub dword esp , -8; restore esp
if164: nop
if162: nop
ret
after160: nop
mov dword eax , 4
mov dword [ esp -8] , eax ; save arg 1
mov dword eax , 4
mov dword [ esp -16] , eax ; save arg 1
mov dword eax , 400
mov dword [ esp -24] , eax ; save arg 1
add dword esp , -16; adjust esp
call fn_seq
sub dword esp , -16; restore esp
mov dword [ esp -20] , eax ; save arg 2
add dword esp , -8; adjust esp
call fn_toggle_nth
sub dword esp , -8; restore esp
mov dword [ esp -12] , eax ; save arg 2
call fn_tog_to_n
ret
