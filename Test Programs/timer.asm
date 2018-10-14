; Timer - loop for some time then produce output

; target processor, tells dasm which processor we want
    processor 6502
    ; code origin
    org $1001

; the basic stub to run the assembly code
stub	    
    dc.w    end
    dc.w    1234
    dc.b    $9e, "4660", 0 ; 1234 in hex base 10 = 4660
end
    dc.w    0   ; program stub

    seg
    org $1234
code 
    jsr $e544   ; clear the screen	
       
    lda #$53    ;heart symbol
    sta $1fb8
    
    lda #$04     ; purple
    sta $97b8
    
    
    ; set up timer and interrupts
    lda $911b
    and #$df   ; timer 2 countdown enabled
    sta $911b
    
    lda #$a0    ; enable timer interrupt
    sta $911e

    ; set timer 2
    lda #$ff     ; 2s 
    sta $9119
    lda #$ff     ; 2s 
    sta $9118    

wait
    jmp wait
    
    rts
 
timer_isr
    pha
 
    ;lda $911d   ; check interupt flags
    ;and #$20

    ;beq timer_isr
    
    lda #$53        ;heart symbol
    ;jsr $ffd2		; display heart
    
    sta $1fcd
    
    lda #$04     ; purple
    sta $97cd
    
    ; set timer; 65535 ms
    lda #$ff     
    sta $9119
    lda #$ff      
    sta $9118   
    

    pla
    
    rti  

 
    seg
    org $fffe
IRQVEC: .word timer_isr