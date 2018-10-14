; Timer - loop for some time then produce output

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
    
    jsr $e544   ; clear the screen	
       
    lda #$53    ;heart symbol
    sta $1fb8
    
    lda #$04     ; purple
    sta $97b8
    
    ; set up timer
    lda $911b
    and #$df   ; timer 2 countdown enabled
    sta $911b
    
    lda #$a0    ; enable timer interrupt
    sta $911e
    
    lda #<timer
    sta $314
    lda #>timer
    sta $315  

    ; set timer
    lda #$ff     ; 2s 
    sta $9119
    lda #$ff     ; 2s 
    sta $9118    
    
    ;jsr timer
    ;jsr timer
    ;jsr timer

    

    
    ; set timer to countdown
    ;lda #$df
    ;sta $1b62
    
    ;lda $911b
    ;and 
    
    ;beq end
    
    rts
    

  
pheart   ;subroutine for printing purple heart

timer  
    lda $911d   ; check interupt flags
    and #$20

    bne timer
    
    lda #$53    ;heart symbol
    sta $1fcd
    
    lda #$04     ; purple
    sta $97cd

    rti
