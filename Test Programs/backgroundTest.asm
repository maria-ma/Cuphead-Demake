; Program: test4.asm
; About: displays black border and screen and will change the text color to white 


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
		lda #$8    	; load black color
		sta $900f   ; store in color border (36879)
		lda #$1		; load white color 
		sta $0286	; store as color code
		
		; display white text
		lda #$54        ; load T
		jsr $ffd2       ; display T
		lda #$45        ; load E
		jsr $ffd2       ; display E
		lda #$53        ; load S
		jsr $ffd2       ; display s
		lda #$54        ; load T
		jsr $ffd2       ; display T
	
		rts
	
; main