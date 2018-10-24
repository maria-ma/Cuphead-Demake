; Program: songtest.asm
; About: plays the first phrase of cuphead's menu song
; basic stub -> make a hexdump to figure out where to dump -> move it back slowly to remove the second org

; integrate sound w rest of game (set up with the interrupts) 
; get infrastructure to do with interrupts (so interrupt service routine to do it) 
; keyboard timer -> rom to use for keyboard, hook w timer and go for timer 
; upload the next thing to the chain

; intervals: take adv of other refs -> 3 bytes to low memory of where rom is keeping track of counter (const inc)
; game loop; pull and use shoot 

; target processor
	processor 6502

; code origin
	org $1001
	dc.w	end
	dc.w	1234
	dc.b	$9e, "4660", 0 ; 1234 in hex base 10 = 4660
end
	dc.w	0	; program stub
	org $1234
	
start
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
; main