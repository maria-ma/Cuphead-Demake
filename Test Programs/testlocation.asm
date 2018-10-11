; Program: testlocation.asm
; About: print in the middle of the screen

    ; target processor, tells dasm which processor we want
    processor 6502
    ; code origin
    seg
    org $1001

; the basic stub to run the assembly code
stub	    
    dc.w    end
    dc.w    1234
    dc.b    $9e, "4660", 0 ; 1234 in hex base 10 = 4660
end
    dc.w    0   ; program stub
    org $1234

main
    ldy data
loop
    lda #$20 ; space
    jsr $ffd2

    iny
    cpy #$fd    ; compare 23*22
    bne loop

    lda data
    cmp #$01
    beq print
    ldx data
    inx
    stx data
    bne loop

print
    lda #$08
    sta $1efc
    lda #$49
    sta $1efd

fin
    bne fin


data
    .byte #$00  ; 0000 0000
