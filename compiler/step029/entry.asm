
extern debug_stack
global scheme_entry
scheme_entry: nop 
push ebp
mov	ebp, esp
mov dword esi , [ esp + 8 ] 
scheme_heap_in_esi: nop
jmp after143
fn_tak: nop
mov dword eax , [ esp -8] 
mov dword [ esp -16] , eax 
mov dword eax , [ esp -4] 
cmp dword [ esp -16] , eax 
mov dword eax , 0 
setl al
shl dword eax , 7
or dword eax , 31
cmp dword eax , 31
je if150
mov dword eax , [ esp -4] 
mov dword [ esp -32] , eax 
mov dword eax , 4
sub dword [ esp -32] , eax 
mov dword eax , [ esp -32] 
mov dword [ esp -32] , eax ; save arg 2
mov dword eax , [ esp -8] 
mov dword [ esp -36] , eax ; save arg 3
mov dword eax , [ esp -12] 
mov dword [ esp -40] , eax ; save arg 4
