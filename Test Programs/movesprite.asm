; move sprite around


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
    jsr $e544		; clear the screen
     
    lda #81
    sta $1ec6
     
    lda #$06     ; blue
    sta $96c6
     
    ;ldy #30720      ; color offset
    jsr $e544		; clear the screen
    lda #81
    sta $1ec7
     
    lda #$06     ; blue
    sta $96c7

    jsr $e544		; clear the screen
    lda #81
    sta $1ec8
     
    lda #$06     ; blue
    sta $96c8
    
    jsr $e544		; clear the screen
    lda #81
    sta $1ec9
     
    lda #$06     ; blue
    sta $96c9
    
    jsr $e544		; clear the screen
    lda #81
    sta $1ed0
     
    lda #$06     ; blue
    sta $96d0
    
    jsr $e544		; clear the screen
    lda #81
    sta $1ed1
     
    lda #$06     ; blue
    sta $96d1
    
    jsr $e544		; clear the screen
    lda #81
    sta $1ed2
     
    lda #$06     ; blue
    sta $96d2
    
    jsr $e544		; clear the screen
    lda #81
    sta $1ed3
     
    lda #$06     ; blue
    sta $96d3
    
    jsr $e544		; clear the screen
    lda #81
    sta $1ed4
     
    lda #$06     ; blue
    sta $96d4
    
    bne code
    
    ; can't branch past this 
    ;jsr $e544		; clear the screen
    ;lda #81
    ;sta $1ed5
     
    ;lda #$06     ; blue
    ;sta $96d5
    
    ;rts
    
    ;30720

    
;data
;    .byte #7680  ; beginning of map  