; Program: shoottest.asm
; About: Prints 'F' when fire is pressed
; set joystick to keyset A, modify it: very middle keyset is fire (?)

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

code
	lda #$00	
	sta $9113
	lda $9111
	eor #$df 	
	bne code
	lda #$46	
	jsr $ffd2
	bne code
