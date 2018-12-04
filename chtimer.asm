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
    lda #255          ; change where it gets its characters from
    sta $9005

    ; set up timer and interrupts
    sei
    
    lda $911b
    and #$df   ; timer 2 countdown enabled
    ;lda #$df
    sta $911b
    
    lda #$a0    ; enable timer interrupt
    sta $911e
    
    ; set timer 2 7000 = $1b58
    lda #$ff     
    sta $9119
    lda #$ff     
    sta $9118  
    
    ;$1b94 - location of irq
    lda #<timer_isr
    sta $0314
    
    lda #>timer_isr
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
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CUPHEAD SHOOT SUBROUTINE            ;
;-------------------------------------;
; Continues drawing bullets if needed ;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
chshoot
    pha
    txa
    pha
    
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
    
    ; Erase previous bullet 
    lda #12
    sta CUPYOFFSET-1,X  ; CUPYOFFSET + X -(Y*22)-1

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
    lda #12     ; also erase last bullet
    sta CUPYOFFSET,X
    
    
crsttime    
    ; Reset timer if not at end 
    lda #99
    sta CHST1
    sta CHST2 
    
    pla
    tax
    pla
    
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BOSS SHOOT SUBROUTINE             ;
;-----------------------------------;
; Actually issues bullets from boss ;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bossshoot
    pha
    txa
    pha
    
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
    sta CUPYOFFSET,X  ; CUPYOFFSET + X -(Y*22)   ;CHANGE AFTER TESTING!!!!!!
    lda #6   ; blue
    sta CUPYOFFSET+SPACECOLOFF,X
    
    ; Erase previous bullet
    lda #12   ; space
    sta CUPYOFFSET+1,X
    

    dec BSHOOT    ; next location
    
    ; Collision resolution first here
    ;dec CHLIVES		; update cuphead's life when hit
	;jsr printlives
    
    ; Otherwise, check if wall reached Check if end of shot; reset bit 0 of BSHOOT   
    lda BSHOOT
    and #$1f
    ;cmp #     ;BOSSPOSI+4
    ;bmi chkboss  ; if not at end, just move on to if boss shoots
    bne brsttime
    lda #0       ; otherwise, clear shoot bit
    sta BSHOOT
    lda #12     ; also erase last bullet
    sta CUPYOFFSET,X   ;CHANGE AFTER TESTING!!!!!!
        
brsttime   
    ; Reset timer if not at end 
    lda #99
    sta BST1
    sta BST2 
    
    pla
    tax
    pla
   
    rts


timer_isr
    pha

    lda $911d   ; check interupt flags
    and #$20
    
    
    beq return
    
    lda $9118    ; read from low order to reset
    lda $9119
    lda #00
    sta $9119
    sta $9118
    
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
    jsr chshoot
    
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
    jsr bossshoot

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
    ;sei
    ;cli
    ;jmp  $fead
    ;jmp $eabf
    rti
    

;;;;;;;;;;;;;;;;;;;;;;;;;
; Character Information ;
;;;;;;;;;;;;;;;;;;;;;;;;;    
    org $1c00  ;64 characters
data
    ;;;;; Cuphead Logo ;;;;;
    ; Char0
    .byte #30,#255,#255,#231,#15,#127,#255,#207
    ; Char1
    .byte #207,#111,#127,#31,#0,#7,#15,#29
    ; Char 2
    .byte #25,#31,#15,#15,#3,#1,#1,#0
    ; Char3
    .byte #0,#6,#15,#15,#0,#0,#0,#0 
    ; Char4
    .byte #0,#0,#0,#0,#255,#255,#255,#255
    ; Char5
    .byte #255,#255,#255,#254,#252,#255,#255,#254     
    ; Char6,
    .byte #254,#254,#255,#255,#255,#239,#239,#198
    ; Char7
    .byte #198,#198,#199,#199,#0,#0,#0,#0    
    ; Char8
    .byte #0,#0,#0,#0,#192,#192,#192,#192
    ; Char9
    .byte #192,#128,#8,#8,#31,#252,#252,#0
    ; Char10
    .byte #0,#192,#224,#224,#0,#0,#0,#0
        
    ; Char 11 - Block
    .byte #255,#255,#255,#255,#255,#255,#255,#255
    
    ; Char 12 - space
    .byte #0,#0,#0,#0,#0,#0,#0,#0

    ; Char 13 = Letter P
    .byte #$0,#$7c,#$42,#$42,#$7c,#$40,#$40,#$40
    
    ; Char 14 = Letter L
    .byte #$0,#$40,#$40,#$40,#$40,#$40,#$40,#$7e
    
    ; Char 15 = Letter A
    .byte #$0,#$18,#$24,#$42,#$7e,#$42,#$42,#$42
    
    ; Char 16 = Letter Y
    .byte #$0,#$22,#$22,#$22,#$1c,#$8,#$8,#$8
    
    ; Char 17 = Letter C
    .byte #$0,#$1c,#$22,#$40,#$40,#$40,#$22,#$1c
    
    ; Char 18 = Letter R
    .byte #$0,#$7c,#$42,#$42,#$7c,#$48,#$44,#$42
    
    ; Char 19 = Letter E
    .byte #$0,#$7e,#$40,#$40,#$7c,#$40,#$40,#$7e
    
    ;Char 20 = Letter D
    .byte #$0,#$78,#$24,#$22,#$22,#$22,#$24,#$78

    ; Char 21 - Letter I
    .byte #$0,#$3e,#$8,#$8,#$8,#$8,#$8,#$3e

    ; Char 22 = Letter T
    .byte #$0,#$3e,#$8,#$8,#$8,#$8,#$8,#$8
    
    ; Char 23 = Letter S
    .byte #$0,#$3c,#$42,#$40,#$3c,#$2,#$42,#$3c
    
    ; Char 24 - Letter N
    .byte #$0,#$42,#$62,#$52,#$4a,#$46,#$42,#$42

    ; Char 25 - Letter M
    .byte #$0, #$41, #$63, #$55, #$49, #$41, #$41, #$41    
    
    ; Char 26 = Arrow Char
    .byte #$0,#$30,#$18,#$c,#$6,#$c,#$18,#$30
    
    ; Char 27 = Heart
    .byte #$0, #$36, #$7f, #$7f, #$7f, #$3e, #$1c, #$8 
    
    ; Char 28 = Bullet
    .byte #$0, #$0, #$0, #$7e, #$0, #$0, #$0, #$0
    
    ; Char 29 = Platform
    .byte #$ff, #$ff, #$7e, #$3c, #$0, #$0, #$0, #$0 
    
    ; Char 30 = Grass
    .byte #$aa, #$ff, #$ff, #$ff, #$ff, #$ff, #$ff, #$ff 
    
    ; Char 31 = Cuphead 1
    .byte #$e0, #$7e, #$42, #$42, #$24, #$7e, #$3c, #$24 
    
    ; Char 32 = Cuphead 2; not used right now
    .byte #0,#0,#0,#0,#0,#0,#0,#0
    
    ; Char 33 to 48 = Small Boss
    .byte #$0, #$0, #$0, #$0, #$0, #$0, #$0, #$1 ;33
    .byte #$0, #$0, #$0, #$3, #$1c, #$60, #$80, #$0 ;34
    .byte #$0, #$0, #$0, #$f8, #$7, #$0, #$0, #$0 ;35
    .byte #$0, #$0, #$0, #$0, #$0, #$c0, #$20, #$10 ;36

    .byte #$2, #$4, #$4, #$8, #$8, #$8, #$10, #$10 ;37
    .byte #$0, #$0, #$0, #$0, #$30, #$10, #$10, #$10 ;38
    .byte #$0, #$0, #$0, #$0, #$6, #$2, #$2, #$2 ;39
    .byte #$8, #$4, #$4, #$2, #$2, #$2, #$1, #$1 ;40

    .byte #$10, #$10, #$10, #$10, #$10, #$8, #$8, #$8  ;41
    .byte #$30, #$0, #$7, #$7, #$0, #$0, #$0, #$2 ;42
    .byte #$6, #$0, #$80, #$80, #$0, #$0, #$1, #$2 ;43
    .byte #$1, #$1, #$1, #$1, #$1, #$2, #$2, #$2 ;44
    
    .byte #$4, #$4, #$2, #$1, #$0, #$0, #$0, #$0 ;45
    .byte #$1, #$0, #$0, #$0, #$80, #$60, #$1c, #$3 ;46
    .byte #$fc, #$0, #$0, #$0, #$0, #$0, #$7, #$f8 ;47
    .byte #$4, #$4, #$8, #$10, #$20, #$c0, #$0, #$0 ;48
    
    ; Char 49 to 69 = Tombstone
    ;1
    .byte #$0, #$0, #$0, #$0, #$0, #$3, #$4, #$9

    ;2      50
    .byte #$10, #$38, #$10, #$10, #$fe, #$1, #$7c, #$83

    ;3
    .byte #$0, #$0, #$0, #$0, #$0, #$80, #$40, #$20

    ;4
    .byte #$12, #$24, #$28, #$49, #$52, #$52, #$52, #$52

    ;5
    .byte #$0, #$7c, #$c6, #$29, #$c6, #$42, #$d6, #$0

    ;6
    .byte #$90, #$48, #$28, #$24, #$94, #$94, #$94, #$94

    ;7      55
    .byte #$52, #$51, #$50, #$50, #$50, #$57, #$54, #$57

    ;8
    .byte #$38, #$45, #$82, #$7c, #$0, #$bb, #$92, #$93

    ;9
    .byte #$94, #$14, #$14, #$14, #$14, #$d4, #$54, #$d4

    ;10
    .byte #$56, #$55, #$54, #$50, #$ff, #$80, #$80, #$ff

    ;11
    .byte #$12, #$12, #$ba, #$0, #$ff, #$0, #$0, #$ff

    ;12     60
    .byte #$14, #$14, #$14, #$14, #$fe, #$2, #$2, #$fe
    