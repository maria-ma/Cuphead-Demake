; draw basic playfield
; testing drawing something meaniningful with VIC Characters

    ; target processor, tells dasm which processor we want
	processor 6502
	; code origin
	; seg
	org $1001
    
    
    ; the basic stub to run the assembly code
	    dc.w    end
    	dc.w    1234
    	dc.b    $9e, "4660", 0 ; 1234 in hex base 10 = 4660
end
    dc.w    0    ; program stub

    org $1234
   
main 
    jsr clear        ; clear screen

    lda #8          ; change to black
    sta $900f

    jsr playfield

    ; store box at starting position 8054
    lda #81               
    sta 8054
        
        ; start at position 0,0
         ldx #0
         stx $0                                 
         stx $1
    
loop    jsr wait
        lda 197
        cmp #9                                  ; w
        beq up                                  ; up
        cmp #17                                 ; a
        beq left                                ; left
        cmp #41                                 ; s
        beq down                                ; down
        cmp #18                                 ; d
        beq right                               ; right
        jmp loop

endloop 
        jsr playfield
        jsr draw
        jmp loop
        
up      ldx $1
        dex                                     ; move up 1
        txa
        cmp #$ff                                ; boundaries
        beq endloop
        ;stx $1     ;commented out so don't move up
        jmp endloop

; be able to move left or right only for now
; assume down is not an option
; to do: fix up so it "jumps?"

left    ldx $0
        dex                                     ; move left
        txa
        cmp #$ff                                ; bounds
        beq endloop
        stx $0
        jmp endloop

right   ldx $0
        inx                     ; move right
        txa
        cmp #$16
        beq endloop
        stx $0
        jmp endloop


down    ldx $1      ;
        inx
        txa
        cmp #$10    ; stop at floor
        beq endloop
        ; stx $1    ;commented out so don't move down
        jmp endloop 

cuphead
    ldx #81        ; cuphead
    stx 8054  
    rts

draw    ldx $1
        ldy #0
        txa 
        cmp #$B
        bcc drawY
        clc
        sbc #$A
        cmp #0
        beq drawX1
        tax
drawY   txa
        cmp #0
        beq drawX1
        clc
        tya
        adc #$16
        tay
        dex
        jmp drawY
drawX1  ldx $0
drawX2  txa
        cmp #0
        beq doneX
        dex
        iny
        jmp drawX2
doneX   ldx $1
        txa
        cmp #$B
        bcs draw2
        jmp draw1
enddraw rts


draw1   jsr clear
        lda #81
        sta 8054,Y
        jmp enddraw
        
draw2   jsr clear
        lda #$81
        sta 7966,Y
        jmp enddraw

wait    ldy #$16                            
reset   ldx #$FF
waitloop    dex
        cpx #$0
        bne waitloop
        dey
        cpy #$0
        bne reset
        rts 


clear   lda #$93                            
        jsr $ffd2                          

playfield
    ;lda #$a2        ; Floor
    lda #102
    sta $1fa2
    sta $1fa3
    sta $1fa4
    sta $1fa5
    sta $1fa6
    sta $1fa7
    sta $1fa8
    sta $1fa9
    sta $1faa
    sta $1fab
    sta $1fac
    sta $1fad
    sta $1fae
    sta $1faf
    sta $1fb0
    sta $1fb1
    sta $1fb2
    sta $1fb3
    sta $1fb4
    sta $1fb5
    sta $1fb6
    sta $1fb7

    
    lda #83        ;lives
    sta $1e17
    sta $1e18
    sta $1e19
    
    ;boss
    ;corners
    lda #122    ;bottom right
    sta $1f9e
    
    lda #76     ;bottom left
    sta $1f99
    
    lda #79     ;upper left
    sta $1f15
    
    lda #80     ;upper right
    sta $1f1a
      
    ;bottom
    lda #100
    sta $1f9a
    sta $1f9b
    sta $1f9c
    sta $1f9d
    
    ;top
    lda #99
    sta $1f16
    sta $1f17
    sta $1f18
    sta $1f19
    
    ;left
    lda #101
    sta $1f83
    sta $1f6d
    sta $1f57
    sta $1f41
    sta $1f2b
    
    ;lda #81
    lda #103
    sta $1f88
    sta $1f72
    sta $1f5c
    sta $1f46
    sta $1f30
    
    ;eyes
    lda #81
    sta $1f42
    sta $1f45

    rts