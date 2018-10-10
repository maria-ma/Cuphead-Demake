; Program: screenshake.asm
; About: shifts the position of the screen several times
; to make a shaking effect

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
	
		jsr $e544   ; clear the screen	
		
		ldx	$ffff		
test	ror	$9000	; shifting horizontal screen value 
		ror	$9001	; shifting vertical screen value 
		jsr	$e544	; clear screen 
		dex
		bmi	test
		jsr	reset	; reset screen to original values
			
		ldx $ffff
test1	asl $9000	; arithmetic shift left
		asl	$9001	
		jsr	$e544
		dex
		bmi	test1		
		jsr	reset

		ldx $ffff
test2	rol $9000	; shift left
		rol	$9001
		jsr	$e544
		dex
		bmi	test2		
		jsr	reset

		ldx $ffff
test3	lsr $9000	; logical shift right
		lsr	$9001
		jsr	$e544
		dex
		bmi	test3		
		jsr	reset
		
		rts			; go back to main 

		; resets screen position to original values
reset	lda	#$c
		sta	$9000
		lda	#$26
		sta	$9001
		jsr	$e544
		nop
		rts
; main