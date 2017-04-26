
extern debug_stack
global scheme_entry
scheme_entry: nop 
push ebp
mov	ebp, esp
mov dword esi , [ ebp + 8 ] 
scheme_heap_in_esi: nop
jmp after144
lambda143: nop ; comp-lambda 
mov dword eax , 4 ; integer 1
push dword eax
mov dword eax , 8 ; integer 2
add dword eax , [ esp ]
ret
after144: nop
mov dword ebx , esi
mov dword [ esi ] , lambda143
add dword esi , 4 
add dword eax , 8
and dword eax , (-8) 
mov dword eax , ebx
or dword eax , 110b 
and dword eax , -8 ; untag closure -8 is binary 11...1111000  lower 3 bits zero
mov dword eax , [eax] ; load CODE address 
call eax ; call closure
mov	esp, ebp
pop	ebp
ret
