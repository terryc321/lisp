
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
mov dword eax , 4 ; integer 1
mov dword [ ebp - 12] , eax  ; let bound a
mov dword eax , 8 ; integer 2
mov dword [ ebp - 16] , eax  ; let bound b
mov dword eax , 12 ; integer 3
mov dword [ ebp - 20] , eax  ; let bound c
mov dword eax , 16 ; integer 4
mov dword [ ebp - 24] , eax  ; let bound d
mov dword eax , 20 ; integer 5
mov dword [ ebp - 28] , eax  ; let bound e
mov dword eax , 24 ; integer 6
mov dword [ ebp - 32] , eax  ; let bound f
mov dword eax , 28 ; integer 7
mov dword [ ebp - 36] , eax  ; let bound g
jmp after144
lambda143: push dword ebp ; comp-lambda 
mov dword ebp , esp
mov dword eax , [ ebp + 8 ] ; move closure ptr into eax
mov dword eax , [ eax + 4] 
mov dword esp , ebp
pop dword ebp
ret
after144: nop
mov dword ebx , esi
mov dword [ esi ] , lambda143
add dword esi , 4 
mov dword eax , [ ebp + 36] 
mov dword [ esi ] , eax
add dword esi , 4 
add dword eax , 8
and dword eax , (-8) 
mov dword eax , ebx
or dword eax , 110b 
and dword eax , -8 ; untag closure -8 is binary 11...1111000  lower 3 bits zero
push dword eax
mov dword eax , [eax] ; load CODE address 
call eax ; call closure
mov	esp, ebp
pop	ebp
ret
