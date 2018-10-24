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
		
chrout = $ffd2
		
main	
		; clear screen
		jsr	clear
		;create playfield
		jsr makePlayfield

		; make the box
		jsr makeBox

		; load black bg
		ldx #$8								
		stx 36879

		;lda #$1		; load white color 
		; sta $0286	; store as color code
		
		; store box at starting position 8054
		lda #$0					
		sta 8054
		
		; start at position 0,0
		 ldx #0
		 stx $0									
		 stx $1
		
loop	jsr wait
		lda 197
		cmp #9									; w
		beq up									; up
		cmp #17									; a
		beq left								; left
		cmp #41									; s
		beq down								; down
		cmp #18									; d
		beq right								; right
		jmp loop

endloop	jsr draw

		jsr makePlayfield
		jmp loop
		
up		ldx $1
		dex										; move up 1
		txa
		cmp #$ff								; boundaries
		beq endloop
		;stx $1 	;commented out so don't move up
		jmp endloop

; be able to move left or right only for now
; assume down is not an option
; to do: fix up so it "jumps?"

left	ldx $0
		dex										; move left
		txa
		cmp #$ff								; bounds
		beq endloop
		stx $0
		jmp endloop

right	ldx $0
		inx						; move right
		txa
		cmp #$16
		beq endloop
		stx $0
		jmp endloop


down	ldx $1		;
		inx
		txa
		cmp #$10 	; stop at floor
		beq endloop
		; stx $1 	;commented out so don't move down
		jmp endloop	

draw	ldx $1
		ldy #0
		txa	
		cmp #$B
		bcc drawY
		clc
		sbc #$A
		cmp #0
		beq drawX1
		tax
drawY	txa
		cmp #0
		beq drawX1
		clc
		tya
		adc #$16
		tay
		dex
		jmp drawY
drawX1	ldx $0
drawX2	txa
		cmp #0
		beq doneX
		dex
		iny
		jmp drawX2
doneX	ldx $1
		txa
		cmp #$B
		bcs draw2
		jmp draw1
enddraw rts


draw1 	jsr clear
		lda #$0
		sta 8054,Y
		jmp enddraw
		
draw2 	jsr clear
		lda #$0
		sta 7966,Y
		jmp enddraw

wait	ldy #$16							
reset	ldx #$FF
waitloop	dex
		cpx #$0
		bne waitloop
		dey
		cpy #$0
		bne reset
		rts				
makeBox	
		ldx data0
		stx 7168
		stx 7169
		stx 7170
		stx 7171
		stx 7172
		stx 7173
		stx 7174
		stx 7175
		
		ldx #$FF	
		stx 36869
		rts
makeLives

	lda #83        ;lives
    sta $1e17
    sta $1e18
    sta $1e19

    rts

makePlayfield

	jsr makeLives
	
    lda data0
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

    rts


clear	lda #$93							
        jsr chrout							
		rts


heart
	.byte #$00
	.byte #$cc
	.byte #$99
	.byte #$81
	.byte #$42
	.byte #$24
	.byte #$18
	.byte #$00

data0
	.byte #$ff
data1
	.byte #$81
data2
	.byte #$7e
zero
	.byte #$00