
extern debug_stack
global scheme_entry
section .data
align 32
toplevel: times 4 dd 0
section .text
align 32
scheme_entry: nop 
push ebp
mov	ebp, esp
mov dword esi , [ ebp + 8 ] 
scheme_heap_in_esi: nop
mov dword eax , 40 ; integer 10
push dword eax
jmp after144
lambda143: nop ; 
push dword ebp ; entry prologue
mov dword ebp , esp
mov dword eax , [ ebp + 8] 
mov dword esp , ebp ; exit prolog
pop dword ebp
ret
after144: nop
mov dword ebx , esi
mov dword [ esi ] , lambda143
add dword esi , 4 
add dword eax , 8
and dword eax , (-8) 
mov dword eax , ebx
or dword eax , 110b 
and dword eax , -8 ; untag closure 
push dword eax ; closure ptr
mov dword eax , [eax] ; load CODE address 
call eax ; call closure
add dword esp , 8
mov	esp, ebp
pop	ebp
ret
