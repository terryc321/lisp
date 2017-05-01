
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
push dword eax
jmp after4077
lambda4076: nop ; 
push dword ebp ; entry prologue
mov dword ebp , esp
mov dword esp , ebp ; exit prolog
pop dword ebp
ret
after4077: nop
mov dword ebx , esi
mov dword [ esi ] , lambda4076
add dword esi , 4 
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
