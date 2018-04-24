# Tiny-Basic interpreter and compiler written in Haskell
Compiles to ARM assembly. Assembly code is not optimized at all. It is just a toy compiler for a toy language.
Wikipedia page on [tiny-basic](https://en.wikipedia.org/wiki/Tiny_BASIC)

# Compilation example

Input file : factorial.bas

~~~~~
GOTO 20

10 LET R = R * N
LET N = N-1
IF N = 0 THEN RETURN
IF N > 0 THEN GOSUB 10
RETURN

20 INPUT N
LET R = 1
GOSUB 10

PRINT "N! = ", R
~~~~~

Output file : factorial.s

~~~~~
.data
.balign 4
R: .word 0
N: .word 0
string1: .asciz "N! = "
pattern: .asciz "%d"
inputN: .asciz "N = "
empty_string: .asciz ""
.text
.global main
gosub:
        push {lr}
goto :
        cmp r0, #20
        beq label_20
        cmp r0, #10
        beq label_10
main:
        push {lr}

# GOTO 20
        mov r0, #20
        push {r0}
        pop {r0}
        b goto

# 10 LET R = R*N
label_10:
        ldr r0, address_of_R
        ldr r0, [r0]
        push {r0}
        ldr r0, address_of_N
        ldr r0, [r0]
        push {r0}
        pop {r1}
        pop {r0}
        mul r0, r0, r1
        push {r0}
        pop {r0}
        ldr r1, address_of_R
        str r0, [r1]

# LET N = N-1
        ldr r0, address_of_N
        ldr r0, [r0]
        push {r0}
        mov r0, #1
        push {r0}
        pop {r1}
        pop {r0}
        sub r0, r0, r1
        push {r0}
        pop {r0}
        ldr r1, address_of_N
        str r0, [r1]

# IF N = 0 THEN RETURN
        ldr r0, address_of_N
        ldr r0, [r0]
        push {r0}
        mov r0, #0
        push {r0}
        pop {r1}
        pop {r0}
        cmp r0, r1
        bne else0
        pop {lr}
        bx lr
else0:

# IF N > 0 THEN GOSUB 10
        ldr r0, address_of_N
        ldr r0, [r0]
        push {r0}
        mov r0, #0
        push {r0}
        pop {r1}
        pop {r0}
        cmp r0, r1
        ble else1
        mov r0, #10
        push {r0}
        pop {r0}
        bl gosub
else1:

# RETURN
        pop {lr}
        bx lr

# 20 INPUT N
label_20:
        ldr r0, address_of_inputN
        bl printf
        ldr r0, address_of_pattern
        ldr r1, address_of_N
        bl scanf

# LET R = 1
        mov r0, #1
        push {r0}
        pop {r0}
        ldr r1, address_of_R
        str r0, [r1]

# GOSUB 10
        mov r0, #10
        push {r0}
        pop {r0}
        bl gosub

# PRINT "N! = ", R
        ldr r0, address_of_string1
        bl printf
        ldr r0, address_of_R
        ldr r0, [r0]
        push {r0}
        ldr r0, address_of_pattern
        pop {r1}
        bl printf
        ldr r0, address_of_empty_string
        bl puts
        pop {lr}
        mov r0, #0
        bx lr
address_of_string1: .word string1
address_of_pattern: .word pattern
address_of_inputN: .word inputN
address_of_empty_string: .word empty_string
address_of_R: .word R
address_of_N: .word N
.global scanf
.global puts
.global printf
~~~~~
