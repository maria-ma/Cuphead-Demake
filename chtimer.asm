; Shooting with interrupts
CHSHOOT EQU $1de0   ; bit0 =y/n shoot; bit1 and 2=yvalue of shot; rest: position along x axis
BSHOOT EQU $1de1

CUPX EQU $1de2  ; Store Cuphead x and Y position    
CUPY EQU $1de3

CHST1 EQU $1de4 ; cuphead bullet timer
CHST2 EQU $1de5

BST1 EQU $1de6  ; boss bullet timer
BST2 EQU $1de7

WORKAREA EQU $1dff
CUPYOFFSET EQU 8076
BOSSPOSI EQU #17

SPACECOLOFF EQU $7800

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
    ; set up timer and interrupts
    sei
    
    lda $911b
    and #$df   ; timer 2 countdown enabled
    ;lda #$df
    sta $911b
    
    lda #$a0    ; enable timer interrupt
    sta $911e
    
    ; set timer 2 7000 = $1b58
    lda #$ff    ; 2s 
    sta $9119
    lda #$ff     ; 2s 
    sta $9118  
    
    ;$1b58 - location of irq
    lda #$58
    sta $0314
    
    lda #$1b
    sta $0315 
    
    lda #$82
    sta CHSHOOT
    
    lda #0 
    sta CHST1
    sta CHST2
        
    lda #$90
    sta BSHOOT
    
    lda #0 
    sta BST1
    sta BST2
    
    cli
    
wait
    ldx $5

    jmp wait

    rts


    
    org $1b58
timer_isr
    pha

    lda $911d   ; check interupt flags
    and #$20
    ;beq return
    
    lda $9118    ; read from low order to reset
    lda #00
    sta $9119
    
    ;beq timer_isr
        
    ;;;;;;;;;;;;;;;;;;
    ; Cuphead Shoot  ;
    ;;;;;;;;;;;;;;;;;;
    ; Create a function that is run if yes; it will handle the x and y of the bullet
    lda CHSHOOT
    and #$80
    
    beq chkboss   ; not shooting, check if boss is shooting
    
    ; Check if CHST time is at 0
    lda CHST1
    beq chst2chk  ; if equal, check next timer
    dec CHST1     ; if not equal, decrement timer and just move on  
    jmp chkboss
    
chst2chk    
    lda CHST2
    beq cisshoot  ; if 0, good to shoot
    dec CHST2     ; if not equal, decrement timer and just move on  
    jmp chkboss    
    
cisshoot
    ;Shooting Position =  X -(Y*22)
    ; Y position
    lda CHSHOOT
    and #$60
    ;sta WORKAREA
    beq cupxshot        ; no y offset
    
    cmp $20         ; pos 1
    bne ynext
    lda #22
    jmp cupxshot

ynext    
    cmp $40         ; pos
    bne ynextxt
    lda #44
    jmp cupxshot

ynextxt    
    lda #66
        
cupxshot  
    sta WORKAREA
    ; X position of shot   
    lda CHSHOOT
    and #$1f    
    ;clc 
    ;adc CUPYOFFSET  ; A = X + CUPYOFSET
    ;sta WORKAREA   
    
    clc
    sbc WORKAREA
    tax
    
    lda #28   ; bullet
    sta CUPYOFFSET,X  ; CUPYOFFSET + X -(Y*22)
    lda #2    ;red
    sta CUPYOFFSET+SPACECOLOFF,X

    inc CHSHOOT    ; next location
       
    ; Collision resolution   
       
       
    ; Check if end of shot; reset bit 0 of CHSHOOT
    lda CHSHOOT
    and #$10
    ;cmp #     ;BOSSPOSI+4
    ;bmi chkboss  ; if not at end, just move on to if boss shoots
    beq crsttime 
    lda #0     ; otherwise, clear shoot bit
    sta CHSHOOT
    
crsttime    
    ; Reset timer if not at end 
    lda #99
    sta CHST1
    sta CHST2 
    
    
    
    ;;;;;;;;;;;;;;;
    ; Boss Shoot  ;
    ;;;;;;;;;;;;;;;
chkboss    
    ; Create a function that runs if yes (probably the boss check one from previous except it won't loop until the bullet is done, just moves it one space
    ; Create a function that is run if yes; it will handle the x and y of the bullet
    lda BSHOOT
    and #$80
    
    beq musicnote   ; not shooting, check if boss is shooting
    
    ; Check if timers are at 0
    lda BST1
    beq bst2chk
    dec BST1
    jmp musicnote

bst2chk
    lda BST2
    beq bisshoot
    dec BST2
    jmp musicnote
    
bisshoot    
    ;Shooting Position =  X - (Y*22)
    ; Y position
    lda BSHOOT
    and #$60
    ;sta WORKAREA
    beq bossxshot        ; no y offset
    
    cmp $20         ; pos 1
    bne bynext
    lda #22
    jmp bossxshot

bynext    
    cmp $40         ; pos
    bne bynextxt
    lda #44
    jmp bossxshot

bynextxt    
    lda #66
        
bossxshot  
    sta WORKAREA
    ; X position of shot   
    lda BSHOOT
    and #$1f    
    ;clc 
    ;adc CUPYOFFSET  ; A = X + CUPYOFSET
    ;sta WORKAREA   
    
    clc
    sbc WORKAREA
    tax
    
    lda #28   ; bullet
    sta CUPYOFFSET+22,X  ; CUPYOFFSET + X -(Y*22)   ;CHANGE AFTER TESTING!!!!!!
    lda #6   ; blue
    sta CUPYOFFSET+22+SPACECOLOFF,X

    dec BSHOOT    ; next location
    
    ; Collision resolution first here
    
    
    ; Otherwise, check if wall reached Check if end of shot; reset bit 0 of BSHOOT   
    lda BSHOOT
    and #$1f
    ;cmp #     ;BOSSPOSI+4
    ;bmi chkboss  ; if not at end, just move on to if boss shoots
    bne brsttime
    lda #0       ; otherwise, clear shoot bit
    sta BSHOOT
        
brsttime   
    ; Reset timer if not at end 
    lda #99
    sta BST1
    sta BST2 


    ; Optional: fancy shooting like a shotgun spread or falling from the sky or ...
    
   
    
    
    ;;;;;;;;;;
    ; Music? ;
    ;;;;;;;;;;
musicnote
    ; Which note (if any) gets played
    
    
    
    ;;;;;;;;;;;;;;;;;;
    ; Boss Movement? ;
    ;;;;;;;;;;;;;;;;;;
    ; Should the boss change positions?
    
    ; Collision resolution
    
    
    
    
    
    
    
    ; set timer; 65535 ms
    lda #$ff     
    sta $9119
    lda #$ff      
    sta $9118   
    
    ; set timer 2
    lda #$07     ; 2s 
    sta $9119
    lda #$d0     ; 2s 
    sta $9118        

return  

    pla
    jmp  $fead