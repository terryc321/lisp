
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
mov dword [ esp -12] , eax 
mov dword eax , 0
cmp dword [ esp -12] , eax 
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
mov dword [ esp -12] , eax 
mov dword eax , [ esp -8] 
mov dword [ esp -24] , eax 
mov dword eax , 4
sub dword [ esp -24] , eax 
mov dword eax , [ esp -24] 
mov dword [ esp -24] , eax ; save arg 2
mov dword eax , [ ebp -4] 
sub dword eax , 110b ; untag closure 
mov dword [esp -20] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -12; adjust stack
call eax ; call closure
sub dword esp , -12; restore esp
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -12] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
if149: nop
ret
after144: nop
mov dword ebx , esi
mov dword [ esi ] , lambda143
add dword esi , 8 
mov dword eax , ebx
add dword eax , 110b 
mov dword [esp -4] , eax 
jmp after151
lambda150: nop
mov dword eax , [ esp -8] 
cmp dword eax , 31
mov dword eax , 0
sete al
shl dword eax , 7
or dword eax , 31
ret
after151: nop
mov dword ebx , esi
mov dword [ esi ] , lambda150
add dword esi , 8 
mov dword eax , ebx
add dword eax , 110b 
mov dword [esp -8] , eax 
jmp after153
lambda152: nop
mov dword eax , [ esp -16] 
cmp dword eax , 47
mov dword eax , 0
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if154
mov dword eax , 47
jmp if155
if154: nop
mov dword eax , [ esp -8] 
mov dword [ esp -20] , eax 
mov dword eax , 4
cmp dword [ esp -20] , eax 
mov dword eax , 0 
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if156
mov dword eax , [ esp -16] 
mov dword eax , [ eax - 1 ] 
mov dword [ esp -28] , eax ; save arg 2
mov dword eax , [ ebp -8] 
sub dword eax , 110b ; untag closure 
mov dword [esp -24] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -16; adjust stack
call eax ; call closure
sub dword esp , -16; restore esp
mov dword [ esp -20] , eax 
mov dword eax , [ esp -12] 
mov dword [ esp -32] , eax ; save arg 2
mov dword eax , [ esp -12] 
mov dword [ esp -36] , eax ; save arg 3
mov dword eax , [ esp -16] 
mov dword eax , [ eax + 3 ] 
mov dword [ esp -40] , eax ; save arg 4
mov dword eax , [ ebp -12] 
sub dword eax , 110b ; untag closure 
mov dword [esp -28] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -20; adjust stack
call eax ; call closure
sub dword esp , -20; restore esp
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -20] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
jmp if157
if156: nop
mov dword eax , [ esp -16] 
mov dword eax , [ eax - 1 ] 
mov dword [ esp -20] , eax 
mov dword eax , [ esp -8] 
mov dword [ esp -32] , eax 
mov dword eax , 4
sub dword [ esp -32] , eax 
mov dword eax , [ esp -32] 
mov dword [ esp -32] , eax ; save arg 2
mov dword eax , [ esp -12] 
mov dword [ esp -36] , eax ; save arg 3
mov dword eax , [ esp -16] 
mov dword eax , [ eax + 3 ] 
mov dword [ esp -40] , eax ; save arg 4
mov dword eax , [ ebp -12] 
sub dword eax , 110b ; untag closure 
mov dword [esp -28] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -20; adjust stack
call eax ; call closure
sub dword esp , -20; restore esp
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -20] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
if157: nop
if155: nop
ret
after153: nop
mov dword ebx , esi
mov dword [ esi ] , lambda152
add dword esi , 8 
mov dword eax , ebx
add dword eax , 110b 
mov dword [esp -12] , eax 
jmp after159
lambda158: nop
mov dword eax , [ esp -8] 
mov dword [ esp -24] , eax ; save arg 2
mov dword eax , [ esp -8] 
mov dword [ esp -28] , eax ; save arg 3
mov dword eax , [ esp -12] 
mov dword [ esp -32] , eax ; save arg 4
mov dword eax , [ ebp -12] 
sub dword eax , 110b ; untag closure 
mov dword [esp -20] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -12; adjust stack
call eax ; call closure
sub dword esp , -12; restore esp
ret
after159: nop
mov dword ebx , esi
mov dword [ esi ] , lambda158
add dword esi , 8 
mov dword eax , ebx
add dword eax , 110b 
mov dword [esp -16] , eax 
jmp after161
lambda160: nop
mov dword eax , [ esp -8] 
mov dword [ esp -16] , eax 
mov dword eax , 400
cmp dword [ esp -16] , eax 
mov dword eax , 0 
setg al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if162
mov dword eax , [ esp -12] 
jmp if163
if162: nop
mov dword eax , [ esp -8] 
mov dword [ esp -24] , eax 
mov dword eax , 4
add dword eax , [ esp -24] 
mov dword [ esp -24] , eax ; save arg 2
mov dword eax , [ esp -8] 
mov dword [ esp -36] , eax ; save arg 2
mov dword eax , [ esp -12] 
mov dword [ esp -40] , eax ; save arg 3
mov dword eax , [ ebp -16] 
sub dword eax , 110b ; untag closure 
mov dword [esp -32] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -24; adjust stack
call eax ; call closure
sub dword esp , -24; restore esp
mov dword [ esp -28] , eax ; save arg 3
mov dword eax , [ ebp -20] 
sub dword eax , 110b ; untag closure 
mov dword [esp -20] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -12; adjust stack
call eax ; call closure
sub dword esp , -12; restore esp
if163: nop
ret
after161: nop
mov dword ebx , esi
mov dword [ esi ] , lambda160
add dword esi , 8 
mov dword eax , ebx
add dword eax , 110b 
mov dword [esp -20] , eax 
jmp after165
lambda164: nop
mov dword eax , [ esp -12] 
cmp dword eax , 47
mov dword eax , 0
sete al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if166
mov dword eax , 47
jmp if167
if166: nop
mov dword eax , [ esp -12] 
mov dword eax , [ eax - 1 ] 
cmp dword eax , 31
je if168
mov dword eax , [ esp -8] 
mov dword [ esp -16] , eax 
mov dword eax , [ esp -8] 
mov dword [ esp -28] , eax 
mov dword eax , 4
add dword eax , [ esp -28] 
mov dword [ esp -28] , eax ; save arg 2
mov dword eax , [ esp -12] 
mov dword eax , [ eax + 3 ] 
mov dword [ esp -32] , eax ; save arg 3
mov dword eax , [ ebp -24] 
sub dword eax , 110b ; untag closure 
mov dword [esp -24] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -16; adjust stack
call eax ; call closure
sub dword esp , -16; restore esp
mov dword [ esi + 4 ] , eax 
mov dword eax , [ esp -16] 
mov dword [ esi ] , eax 
mov dword eax , esi 
inc dword eax
add dword esi , 8
jmp if169
if168: nop
mov dword eax , [ esp -8] 
mov dword [ esp -24] , eax 
mov dword eax , 4
add dword eax , [ esp -24] 
mov dword [ esp -24] , eax ; save arg 2
mov dword eax , [ esp -12] 
mov dword eax , [ eax + 3 ] 
mov dword [ esp -28] , eax ; save arg 3
mov dword eax , [ ebp -24] 
sub dword eax , 110b ; untag closure 
mov dword [esp -20] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -12; adjust stack
call eax ; call closure
sub dword esp , -12; restore esp
if169: nop
if167: nop
ret
after165: nop
mov dword ebx , esi
mov dword [ esi ] , lambda164
add dword esi , 8 
mov dword eax , ebx
add dword eax , 110b 
mov dword [esp -24] , eax 
mov dword eax , 4
mov dword [ esp -36] , eax ; save arg 2
mov dword eax , 4
mov dword [ esp -48] , eax ; save arg 2
mov dword eax , 400
mov dword [ esp -60] , eax ; save arg 2
mov dword eax , [ ebp -4] 
sub dword eax , 110b ; untag closure 
mov dword [esp -56] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -48; adjust stack
call eax ; call closure
sub dword esp , -48; restore esp
mov dword [ esp -52] , eax ; save arg 3
mov dword eax , [ ebp -20] 
sub dword eax , 110b ; untag closure 
mov dword [esp -44] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -36; adjust stack
call eax ; call closure
sub dword esp , -36; restore esp
mov dword [ esp -40] , eax ; save arg 3
mov dword eax , [ ebp -24] 
sub dword eax , 110b ; untag closure 
mov dword [esp -32] ,eax  ; raw closure ptr 
mov dword eax , [eax] ; load procedure ptr from raw closure 
add dword esp , -24; adjust stack
call eax ; call closure
sub dword esp , -24; restore esp
mov	esp, ebp
pop	ebp
ret
