; draw basic playfield
; testing drawing something meaniningful with VIC Characters
SPACECOLOFF EQU $7800  ; difference between location in space and it's color location

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
	jsr song

    ;lda #8          ; change to black
    lda #184          ; change to light cyan
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
        
        ; check if pressed shoot button
        lda #$00	
        sta $9113
        lda $9111
        eor #$df 	
        beq shoot
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
        
shoot       
    ; PLAY SHOOT SOUND EFFECT
    lda #$0f	; vol 15
	sta $900e	; store in vol mem (36878)
    ; Loop and decrement 254 to 128 
    lda #$80    ; load 128 into acc
    sta $1b58   ; store at mem loc 7000; used for comparison later
    ; put 254 in x
    ldx #$fe
shootloop
    stx $900c   ; put in third speaker
    ldy    #$ff   ; make note last longer
shoottimer   ; make the notes last a little longer
    dey
    bne shoottimer
    sty $900c   ; store 0 in third speaker after
    iny
    dex         ; dec x by 1    
    txa         ; for check
    sbc $1b58   ; check if at 128
    bne shootloop
    inx
    txa    
    
    ;DRAW BULLET
    ldx $0
    ldy $1
    
    ;lda 
    
    
    
    jmp loop

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
    
    ;color floor
    lda #5
    sta $97a2
    sta $97a3
    sta $97a4
    sta $97a5
    sta $97a6
    
    sta $97a7
    sta $97a8
    sta $97a9
    sta $97aa
    sta $97ab
    
    sta $97ac
    sta $97ad
    sta $97ae
    sta $97af
    sta $97b0
    
    sta $97b1
    sta $97b2
    sta $97b3
    sta $97b4
    sta $97b5
    
    sta $97b6
    sta $97b7

    ; lives
    ; char
    lda #83        
    sta $1e17    
    sta $1e18
    sta $1e19
    ;color
    lda #2
    sta $1e17+SPACECOLOFF    
    sta $1e18+SPACECOLOFF
    sta $1e19+SPACECOLOFF
    
    
    
    ;boss
    ;corners
    lda #122    ;bottom right
    sta $1f9e
    lda #6
    sta $1f9e+SPACECOLOFF
    
    lda #76     ;bottom left
    sta $1f99
    lda #6
    sta $1f99+SPACECOLOFF
    
    lda #79     ;upper left
    sta $1f15
    lda #6
    sta $1f15+SPACECOLOFF
    
    lda #80     ;upper right
    sta $1f1a
    lda #6
    sta $1f1a+SPACECOLOFF
      
    ;bottom
    lda #100
    sta $1f9a
    sta $1f9b
    sta $1f9c
    sta $1f9d
    ;color
    lda #6
    sta $1f9a+SPACECOLOFF
    sta $1f9b+SPACECOLOFF
    sta $1f9c+SPACECOLOFF
    sta $1f9d+SPACECOLOFF
    
    ;top
    lda #99
    sta $1f16
    sta $1f17
    sta $1f18
    sta $1f19
    ;color
    lda #6
    sta $1f16+SPACECOLOFF
    sta $1f17+SPACECOLOFF
    sta $1f18+SPACECOLOFF
    sta $1f19+SPACECOLOFF
    
    ;left
    lda #101
    sta $1f83
    sta $1f6d
    sta $1f57
    sta $1f41
    sta $1f2b
    ;color
    lda #6
    sta $1f83+SPACECOLOFF
    sta $1f6d+SPACECOLOFF
    sta $1f57+SPACECOLOFF
    sta $1f41+SPACECOLOFF
    sta $1f2b+SPACECOLOFF
    
    ;lda #81
    lda #103
    sta $1f88
    sta $1f72
    sta $1f5c
    sta $1f46
    sta $1f30
    ;color
    lda #6
    sta $1f88+SPACECOLOFF
    sta $1f72+SPACECOLOFF
    sta $1f5c+SPACECOLOFF
    sta $1f46+SPACECOLOFF
    sta $1f30+SPACECOLOFF   
    
    ;eyes
    lda #81
    sta $1f42
    sta $1f45
    ; color
    lda #0
    sta $1f42+SPACECOLOFF 
    sta $1f45+SPACECOLOFF 
    rts
	
song
	lda	#$0f
	sta	$900e	; set speaker volume to max 
	
	; 900b = speaker 2
	; 900c = speaker 3
	; quarter note = 250 (fa) / eighth note = 125 (7d) / half note = 1000 (3e8
	jsr	playa
	jsr	playa
	jsr playbf
	jsr	pause
	jsr playbf
	jsr	pause
	jsr playbf
	jsr playbf
	jsr	playg
	jsr	playg
	jsr	playf
	jsr	pause
	jsr	playf
	jsr	pause
	jsr	playf
	jsr	playf
	jsr	playa
	jsr	playa
	jsr playbf
	jsr	pause
	jsr playbf
	jsr	pause
	jsr playc
	jsr playc
	jsr	pause
	jsr playc
	jsr playc
	jsr	pause
	jsr playb
	jsr playb
	jsr playb
	jsr playb
	
	lda	#$0
	sta	$900e
	sta	$900c
	rts			; go back to main 

playa
	lda	#$da	; note a (218)
	sta	$900c	; play in speaker 2 )
	ldy	#$fa	; duration 
	jsr	play
	rts
	
playbf	
	lda	#$dc	; note b flat (220)
	sta	$900c	; play in speaker 2 )
	ldy	#$7d	; duration 
	jsr	play	
	rts

playb
	lda	#$de	; note b flat (220)
	sta	$900c	; play in speaker 2 )
	ldy	#$7d	; duration 
	jsr	play	
	rts
	
playc
	lda	#$e0	; note b flat (224)
	sta	$900c	; play in speaker 2 )
	ldy	#$7d	; duration 
	jsr	play	
	rts
	
	
playf
	lda	#$d0	; note a (214)
	sta	$900c	; play in speaker 2 )
	ldy	#$fa	; duration 
	jsr	play
	rts
	
playg
	lda	#$d6	; note a (214)
	sta	$900c	; play in speaker 2 )
	ldy	#$fa	; duration 
	jsr	play
	rts

playgf
	lda	#$d3	; note a (214)
	sta	$900c	; play in speaker 2 )
	ldy	#$fa	; duration 
	jsr	play
	rts	
	
pause
	lda	#$0
	sta	$900c
	ldy	#$7f	; duration 
	jsr	play
	rts
; plays note
play
	lda	#$20
	jsr	$ffd2	; print if right if pressed
	dey
	bne	play
	rts