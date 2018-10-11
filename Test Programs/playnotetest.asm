; Program: playnotetest.asm
; About: plays note g continually

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
code 	
	lda #$0b	; vol 11
	sta $900e	; store in vol mem
	lda #$af	; g note = hex(175)
	sta $900a	; speaker 1
	BNE code