; Program: test5.asm
; About: testing the joystick movement buttons (up/down/left/right)
; changes background color based on joystick button pressed 
; prints out which button is pressed

; target processor
	processor 6502

; code origin
	org $1001
	dc.w	end
	dc.w	1234
	dc.b	$9e, "4660", 0 ; 1234 in hex base 10 = 4660
end
	dc.w	0	; program stub
	
start
		org $1234
	
		;jsr $e544   ; clear the screen	
readJS	lda	#$00	; load a 0 
		sta	$9113	; set the DDR for i/o port a for input
				
		;lda	#$7f	; load 0111 1111 (127)
		sta $9122	; set one bit for input and rest for output in DDR port b
		
		lda	$9120	; load output register b (right button)
		eor	#$7f	; and with 128 (get first bit)
		beq	right
		
		lda	$9111	; load output register a (up/down/left/fire)
		eor	#$fb	; check switch 0 (up)
		beq up
		lda $9111
		eor #$f7	; check switch 1 (down)
		beq	down
		lda	$9111
		eor	#$ef	; check switch 2 (left) 
		beq left
		bne readJS
		
up		lda #$1a	; load red border, white screen 
		sta $900f   ; store in color border (36879)
		lda	#$55	; "U" 
		jsr	$ffd2	; print if right if pressed
		bne	readJS

down	lda #$1c	; load purple border, white screen 
		sta $900f   ; store in color border (36879)
		lda	#$44	; "D" 
		jsr	$ffd2	; print if right if pressed
		bne	readJS
		
left	lda #$1f	; load yellow border, white screen 
		sta $900f   ; store in color border (36879)
		lda	#$4c	; "L" 
		jsr	$ffd2	; print if right if pressed
		bne	readJS
		
right	lda #$18   	; load black border, white screen
		sta $900f   ; store in color border (36879)
		lda	#$52	; "R" 
		jsr	$ffd2	; print if right if pressed
		bne	readJS

	
; main