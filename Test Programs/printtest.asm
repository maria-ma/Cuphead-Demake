; Program: printtest.asm
; About: Print "Hello World!" to the screen

	; target processor, tells dasm which processor we want
	processor 6502
	; code origin
	seg
	org $1001

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

print 	jsr $e544		; clear the screen

		lda #$48		; load H
		jsr $ffd2		; display H 

		lda #$45		; load E
		jsr $ffd2		; display E

		lda #$4c		; load L
		jsr $ffd2		; display L

		jsr $ffd2		; display L

		lda #$4f		; load O
		jsr $ffd2		; display O

		lda #$20		; load space
		jsr $ffd2       ; display space

		lda #$57		; load W 
		jsr $ffd2       ; display W	

		lda #$4f		; load O
		jsr $ffd2       ; display O	

		lda #$52		; load R 
		jsr $ffd2       ; display R	

		lda #$4c		; load L 
		jsr $ffd2       ; display L	

		lda #$44		; load D 
		jsr $ffd2       ; display D	

		lda #$21		; load !
		jsr $ffd2		; display !
	
		rts             ; return from subroutine