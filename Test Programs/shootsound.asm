; Translation of: POKE 36878,15:FOR L = 254 TO 128 STEP-1:POKE 36876, L:POKE 36876,0:NEXT:POKE 36878,0

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
    lda #$0f	; vol 15
	sta $900e	; store in vol mem (36878)

    ; Loop and decrement 254 to 128 
    lda #$80    ; load 128 into acc
    sta $1b58   ; store at mem loc 7000; used for comparison later
    
    ; put 254 in x
    ldx #$fe
    
    
    
    ; put 0 in y
    ;ldy #$00
    
loop
    stx $900c   ; put in third speaker
    
    ldy    #$ff   ; make note last longer
    
timer   ; make the notes last a little longer
    dey
    
    bne timer
    
    
    
    sty $900c   ; store 0 in third speaker after
    
    iny
    
    dex         ; dec x by 1    
    
    txa         ; for check
    
    sbc $1b58   ; check if at 128
    
    bne loop

    inx
    
    txa 
