; TinyRAM V=1.000 W=16 K=16
read r1, 0
mov r0, r1
read r1, 0
cjmp 28
cmpg r1, r0
cjmp 4
jmp 8
answer r0
