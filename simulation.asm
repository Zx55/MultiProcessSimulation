; Multiprocess simulation
assume cs:code

data segment
    dw 0, 0                                     ; store original int 9 code
    db 0                                        ; flag shows which function to run
                                                ; 0 -> funcA
                                                ; 1 -> funcB
                                                ; 2 -> funcC
    dw 0                                        ; temporary buffer
data ends

code segment
aStack:
        db 128 dup (0)
bStack:
        db 128 dup (0)
cStack:
        db 128 dup (0)

; snopshot structure:
;          ES      DI SI BP DX CX BX DS AX IP            CS F  SP
aSnopshot:
        dw 0b800h, 0, 0, 0, 0, 0, 0, 0, 0, offset funcA, 0, 0, offset aStack + 128, 3 dup(0)
bSnopshot:
        dw 0b800h, 0, 0, 0, 0, 0, 0, 0, 0, offset funcB, 0, 0, offset bStack + 128, 3 dup(0)
cSnopshot:
        dw 0b800h, 0, 0, 0, 0, 0, 0, 0, 0, offset funcC, 0, 0, offset cStack + 128, 3 dup(0)

start:
        mov ax, cs
        mov ds, ax
        mov ss, ax
        mov sp, offset aStack + 128             ; set stack of funcA
        mov dx, 0                               ; set counter = 0

        call init
        call funcA                              ; start with funcA

; Function A
funcA:
        call delay
        inc dx
        mov ax, dx
        call num2char

        mov si, 5 * 160 + 40 * 2
        call print
        jmp funcA


; Function B
funcB:
        call delay
        inc dx
        mov ax, dx
        call num2char

        mov si, 6 * 160 + 40 * 2
        call print
        jmp funcB


; Function C
funcC:
        call delay
        inc dx
        mov ax, dx
        call num2char

        mov si, 7 * 160 + 40 * 2
        call print
        jmp funcC


; Transform number to char
; Parameters:
;   - (ax) = number
; Return:
;   - (cl) = units
;   - (ch) = tens
num2char:
        push bx

        mov bl, 10

        div bl
        mov cl, ah
        xor ah, ah
        div bl
        mov ch, ah

        pop bx
        ret


; Print numbers on the screen
; Parameter:
;   - (es:si) = video memory
;   - (cl)    = units
;   - (ch)    = tens
; Return:
;   - none
print:
        add ch, '0'
        add cl, '0'

        mov byte ptr es:[si], ch
        mov byte ptr es:[si + 1], 21h
        mov byte ptr es:[si + 2], cl
        mov byte ptr es:[si + 3], 21h

        ret


; Delay
; Parameter:
;   - none
; Return:
;   - none
delay:
        push ax
        push cx

        mov cx, 3
    s1:
        mov ax, 0ffffh
    s2:
        dec ax
        jnz s2
        dec cx
        jnz s1

        pop cx
        pop ax
        ret


; Install the interruption code and initalize the program
; Parameter:
;   - none
; Return:
;   - none
init:
        mov si, offset bSnopshot                ; set address of funcB
        mov [si + 20], cs

        mov si, offset cSnopshot                ; set address of funcA
        mov [si + 20], cs

        mov ax, data
        mov ds, ax
        mov ax, 0
        mov es, ax

        cli                                     ; mask the outer interruption
        mov ax, es:[9 * 4]                      ; store original int 9 code
        mov ds:[0], ax
        mov ax, es:[9 * 4 + 2]
        mov ds:[2], ax

        mov word ptr es:[9 * 4], offset int9    ; set interrupt vector
        mov es:[9 * 4 + 2], cs
        sti

        mov ax, 0b800h                          ; pointer to video memory
        mov es, ax

        ret


; Restore original int 9 code
; Parameters:
;   - none
; Return:
;   - none
restore:
        mov ax, data
        mov ds, ax
        mov ax, 0
        mov es, ax

        cli
        mov ax, ds:[0]
        mov es:[9 * 4], ax
        mov ax, ds:[2]
        mov es:[9 * 4 + 2], ax
        sti

        ret

; Switch the function according to keyboard input
; Parameter:
;   - none
; Return:
;   - none
int9:
        cli
        push ax
        push ds

        mov ax, data
        mov ds, ax

        in al, 60h

        pushf
        call dword ptr ds:[0]

        cmp al, 1eh                             ; 'A' -> funcA
        je reponseA
        cmp al, 30h                             ; 'B' -> funcB
        je reponseB
        cmp al, 2eh                             ; 'C' -> funcC
        je reponseC
        cmp al, 10h                             ; 'Q' -> quit
        je reponseQ

    int9ret:
        pop ds
        pop ax
        sti
        iret

    reponseA:
        cmp byte ptr ds:[4], 0
        je int9ret                              ; return if function doesn't change
        call saveAll                            ; save snopshot
        mov byte ptr ds:[4], 0                  ; change function type
        call switch                             ; switch function

    reponseB:
        cmp byte ptr ds:[4], 1
        je int9ret
        call saveAll
        mov byte ptr ds:[4], 1
        call switch

    reponseC:
        cmp byte ptr ds:[4], 2
        je int9ret
        call saveAll
        mov byte ptr ds:[4], 2
        call switch

    reponseQ:
        sti
        call restore
        mov ax, 4c00h
        int 21h


; Save registers of origin function
; Parameters:
;   - (ds:[4]) = function type to be store
; Return:
;   - none
saveAll:
        pop ds:[5]                              ; save ip

        push bx                                 ; store snopshot in the stack
        push cx
        push dx
        push bp
        push si
        push di
        push es

        mov ax, cs
        mov es, ax
        mov cx, 12

        cmp byte ptr ds:[4], 0
        je saveA
        cmp byte ptr ds:[4], 1
        je saveB
        cmp byte ptr ds:[4], 2
        je saveC

    save:
        pop es:[bx]                             ; save snopshot
        add bx, 2
        loop save
        mov es:[bx], sp                         ; save sp

        push ds:[5]                             ; restore save address
        ret

    saveA:
        mov bx, offset aSnopshot
        jmp save

    saveB:
        mov bx, offset bSnopshot
        jmp save

    saveC:
        mov bx, offset cSnopshot
        jmp save


; Restore registers of function to be switch
; Parameters:
;   - (ds:[4]) = function type to be switch
; Return:
;   - none
switch:
        mov ax, cs
        mov es, ax

        cmp byte ptr ds:[4], 0
        je switchA
        cmp byte ptr ds:[4], 1
        je switchB
        cmp byte ptr ds:[4], 2
        je switchC

    switchStack:
        mov ax, cs
        mov ss, ax                              ; restore stack
        mov sp, es:[bx + 24]

        push es:[bx + 20]                       ; push switch address
        push es:[bx + 18]

        mov cx, 9

    switchReg:
        push es:[bx]                            ; get registers from snopshot
        add bx, 2
        loop switchReg
        push es:[bx + 4]                        ; get flags from snopshot

        popf                                    ; restore registers and flags
        pop ax
        pop ds
        pop bx
        pop cx
        pop dx
        pop bp
        pop si
        pop di
        pop es

        sti
        retf                                    ; (cs:ip) point to function to be switch

    switchA:
        mov bx, offset aSnopshot
        jmp switchStack

    switchB:
        mov bx, offset bSnopshot
        jmp switchStack

    switchC:
        mov bx, offset cSnopshot
        jmp switchStack

code ends

end start