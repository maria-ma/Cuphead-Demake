; draw basic playfield
; testing drawing something meaniningful with VIC Characters
SPACECOLOFF EQU $7800  ; difference between location in space and it's color location
CUPYOFFSET EQU 8076
CUPXOFFSET EQU 8054
ROWDIFF EQU 22

; For drawing start screen
CUPHEADSTART EQU 8064
CUPSTART EQU 7751
HEADSTART EQU 7881
OPTIONSTART EQU 8041
CREDITSTART EQU OPTIONSTART+67

; Boss Timer 2 Info - Do not use for anything else!
TIMERCOUNT1 EQU 7167
TIMERCOUNT2 EQU 7166

; Boss Start
BOSSSTART EQU 8027
TOMBSTART EQU 8027

; Collision Resolution Stuff
BOSSPOSI EQU 17   ; where the boss is on the playing field

; Lives
CHLIVES EQU $0312
BOSSLIVES EQU $0313
BOSSHALF EQU $0334

; Displaying Results
MSGSTART EQU 7814
CUPMSGSTART EQU MSGSTART+66+2
WORDSTART EQU MSGSTART +44+6

; Interrupt Equates
CHSHOOT EQU $1de8   ; bit0 =y/n shoot; bit1 and 2=yvalue of shot; rest: position along x axis
BSHOOT EQU $1de9

;CHST1 EQU $1dea  ; cuphead bullet timer
;CHST2 EQU $1deb

;BST1 EQU $1dec  ; boss bullet timer
;BST2 EQU $1ded

WORKAREA EQU $1dee


; Could have equates for colors

    ; target processor, tells dasm which processor we want
	processor 6502
	; code origin
	; seg
	org $1001
    
    ; the basic stub to run the assembly code
	    dc.w    end
    	dc.w    1010    ; from looking at memory, try memory location $1010
    	dc.b    $9e, "4112", 0 ; 1010 in hex base 10 = 4112
end
    dc.w    0    ; program stub

main 
    jsr clear        ; clear screen

    jsr disstartscreen      ; display start screen   
	;jsr	song		; play the title song
    
    jsr disoptions   ; display the game's options for selection  
    
    ldx #0      ; on "PLAY" option    
optionchoiceloop
    jsr wait
    jsr wait
    lda 197                                 ; current key pressed
    cmp #9                                  ; w
    beq upperop                             ; up

    cmp #41                                 ; s
    beq lowerop                             ; down

    cmp #32                                 ; space		
    beq select

    jmp optionchoiceloop
    
upperop    
    ; If at play, don't move up
    txa
    cmp #0
    beq optionchoiceloop
    
    ; Otherwise at credits so move up
    ; Delete previous arrow
    lda #12     
    sta OPTIONSTART+2*ROWDIFF-1
    ; Draw New Arrow
    lda #26
    sta OPTIONSTART-1
    
    dex
    jmp optionchoiceloop

lowerop
    ; If at credits, don't move down
    txa
    cmp #1
    beq optionchoiceloop
    
    ; Otherwise at play, move down
    cmp #0
    ; Delete Previous Arrow
    lda #12
    sta OPTIONSTART-1
    ; Draw New Arrow
    lda #26
    sta OPTIONSTART+2*ROWDIFF-1
    
    inx
    jmp optionchoiceloop
    
select
    ; If on credits, display credits
    cpx #1
    bne continue
    jsr discredits
    jmp optionchoiceloop

continue    
    jsr wait
    jsr wait
    
    ; set up timer and interrupts
    sei
    
    lda $911b
    and #$df   ; timer 2 countdown enabled
    ;lda #$df
    sta $911b
    
    lda #$a0    ; enable timer interrupt
    sta $911e
    
    ;$1b94 - location of irq
    lda #<timer_isr
    sta $0314
    
    lda #>timer_isr
    sta $0315 
          
    lda #0 
    ;sta CHST1
    ;sta CHST2
    sta BSHOOT
    sta CHSHOOT
    ;sta BST1
    ;sta BST2
    
    ; set timer 2 to max
    lda #$ff     
    sta $9119
    lda #$ff     
    sta $9118
    
    cli
    
    
    
    
    lda #184          ; change to light cyan
    sta $900f
	
    lda #3			; initiate lives
    sta CHLIVES
	sta BOSSHALF	; to know when to transition boss stages
	asl 			; initiate boss lives
	sta BOSSLIVES
    
    ; set up for boss check
    lda #$99
    sta TIMERCOUNT1   
    sta TIMERCOUNT2
    
    jsr clear

    jsr playfield
    
    
    
    
    ;jsr distombstone
    
    ;jsr diswindead
   
    ; store cuphead at starting position 8076
    ldx #31    
    stx 8076
    ldx #2     ; red
    stx 8076+SPACECOLOFF    
        
        ; start at position 0,0
         ldx #0
         stx $0     ; x coord                            
         stx $1     ; y coord
    
loop    ; Check if boss shoots
        jsr boss_shoot_check
		jsr boss_life_check
        
        jsr wait
        lda 197                                 ; current key pressed
        cmp #17                                 ; a
        beq left                                ; left
        cmp #18                                 ; d
        beq right                               ; right
        cmp #9                                  ; w
        beq jump                                ; up
        ; check if pressed shoot button
        cmp #32                                 ; space		
        beq shoot
        jmp loop
        
jump
; animated jump routine
; doesn't go on platform
; doesn't animate when boss shoots (problemo)

        ; removes previous cuphead when jump is pressed
        ldx $0         
		lda #12					
		sta CUPYOFFSET,X		; TODO: make a jump to the clouds

        ; make jumping cuphead
        lda #31
        sta CUPXOFFSET,X

        ; store color of jumping cuphead
        lda #2
        sta CUPXOFFSET+SPACECOLOFF,X

        ; wait a bit
        jsr wait
        jsr wait
        jsr wait
        jsr wait

        ; remove jumping cuphead
        lda #12
        sta CUPXOFFSET,X

        ; check place on screen?
        lda 211                 ; position of cursor?
        cmp #5,X                ; checking every 5st position, want single spot?
        beq jump1

        jmp endloop

jump1
        ldx $0
        lda #12
        sta CUPYOFFSET,Y
        sta CUPXOFFSET,Y
        jsr chplaceholder 
        jmp endloop

endloop 
        ldx $0
        lda #12
        sta CUPYOFFSET-1,X   
        sta CUPYOFFSET+1,X          
        
        jsr draw
        jmp loop


; be able to move left or right only for now
; assume down is not an option
; to do: fix up so it "jumps?"

left    ldx $0
        dex                                     ; move left
        txa
        cmp #$ff                                ; bounds
        beq endloop
        stx $0
        jmp endloop

right   ldx $0
        inx                     ; move right
        txa
        cmp #$10
        beq endloop
        stx $0
        jmp endloop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Boss Life Check Subroutine                         ;
;---------------------------------------------------;
; checks the boss's remaining hp                    ;
; to see if it should transition to its second stage;
; or if it has died yet                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		
boss_life_check
	lda BOSSLIVES
	cmp BOSSHALF
	beq boss_half
	cmp	#0
	beq boss_dead
	rts
	
boss_half
	jsr distombstone
	rts

boss_dead
	jsr diswindead
	rts
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CUPHEAD SHOOT SUBROUTINE ;
; args: none               ;
; returns: nothing         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
shoot       
    pha     ; save registers
    txa
    pha 
    tya 
    pha
    
    lda CHSHOOT
    bne skipshoot
    
    ; PLAY SHOOT SOUND EFFECT
    lda #$0f	; vol 15
	sta $900e	; store in vol mem (36878)
    ; Loop and decrement 254 to 128 
    lda #$80    ; load 128 into acc
    sta 7165  ; store at mem loc 7165; used for comparison later
    ; put 254 in x
    ldx #$fe
shootloop
    stx $900c   ; put in third speaker
    ldy    #$ff   ; make note last longer
shoottimer   ; make the notes last a little longer
    dey
    bne shoottimer
    sty $900c   ; store 0 in third speaker after
    iny
    dex         ; dec x by 1    
    txa         ; for check
    sbc $7167   ; check if at 128
    bne shootloop
    inx
    txa    
    
    ; Set CHSHOOT
    ; Should be 1yyxxxxx
    lda $1
    asl
    asl
    asl
    asl
    asl
    
    clc
    adc #$80
    
    clc
    adc $0
    
    clc
    adc #3
    ; 1 y<<5 x
    ;lda #$82
    ;clc
    ;adc $0
    
    sta CHSHOOT
    
    ; Set counters
    ;lda #0 
    ;sta CHST1
    ;sta CHST2
    
skipshoot
    pla     ; load registers
    tay
    pla
    tax
    pla

    jmp loop

;;;;;;;;;;;;;;;;;;
; DRAW SUBROUTINE;
;;;;;;;;;;;;;;;;;;    
draw    
        pha     ; save registers
        txa
        pha 
        tya 
        pha

        ldx $1
        ldy #0
        txa 
        cmp #$B
        bcc drawY
        clc
        sbc #$A
        cmp #0
        beq drawX1
        tax
drawY   txa
        cmp #0
        beq drawX1
        clc
        tya
        adc #$16
        tay
        dex
        jmp drawY
drawX1  ldx $0
drawX2  txa
        cmp #0
        beq doneX
        dex
        iny
        jmp drawX2
doneX   ldx $1
        txa
        cmp #$B
        bcs draw2
        jmp draw1
enddraw 
        pla     ; load registers
        tay
        pla
        tax
        pla
        
        rts


draw1   
        lda #31
        sta 8076,Y
        
        ; add color
        lda #2
        sta 8076+SPACECOLOFF,Y
        
        jmp enddraw
        
draw2  
        lda #31
        sta 7966,Y
        
        ; add color
        lda #2
        sta 7966+SPACECOLOFF,Y
        
        jmp enddraw

wait    
        pha     ; save registers
        txa
        pha
        tya
        pha

        ldy #$16
reset   ldx #$FF
waitloop    dex
        cpx #$0
        bne waitloop
        dey
        cpy #$0
        bne reset
        
        pla     ; load registers
        tay
        pla
        tax
        pla
        
        rts


clear
        pha
        lda #$93
        jsr $ffd2
        pla
		rts

;;;;;;;;;;;;;;;;;;;;;;;;
; PLAYFIELD SUBROUTINE ;
; Args: None           ;
; Returns: Nothing     ;
;;;;;;;;;;;;;;;;;;;;;;;;            
playfield
    pha             ; Save Acc and x
    txa
    pha
    
    ; Print spaces everywhere
    lda #12
    
    ldx #255
printspaces21
    sta 7679,X
    dex
    bne printspaces21
    
    ldx #251
printspaces23
    sta 7934,X
    dex
    bne printspaces23
    
    ldx #0
    jsr printfloor
    ldx #ROWDIFF
    jsr printfloor
    ldx #ROWDIFF*2
    jsr printfloor
    ldx #ROWDIFF*3
    jsr printfloor
	
	jsr printlives

printboss
    ;boss
    lda #33     ;row 1
    sta BOSSSTART
    lda #34     
    sta BOSSSTART+1
    lda #35     
    sta BOSSSTART+2
    lda #36     
    sta BOSSSTART+3
    
    lda #37     ;row 2
    sta BOSSSTART+ROWDIFF
    lda #38     
    sta BOSSSTART+1+ROWDIFF
    lda #39     
    sta BOSSSTART+2+ROWDIFF
    lda #40     
    sta BOSSSTART+3+ROWDIFF
    
    lda #41     ;row 3
    sta BOSSSTART+2*ROWDIFF
    lda #42     
    sta BOSSSTART+1+2*ROWDIFF
    lda #43     
    sta BOSSSTART+2+2*ROWDIFF
    lda #44     
    sta BOSSSTART+3+2*ROWDIFF
    
    lda #45     ;row 4
    sta BOSSSTART+3*ROWDIFF
    lda #46     
    sta BOSSSTART+1+3*ROWDIFF
    lda #47     
    sta BOSSSTART+2+3*ROWDIFF
    lda #48     
    sta BOSSSTART+3+3*ROWDIFF

    ;Boss Color
    lda #6
    sta BOSSSTART+SPACECOLOFF
    sta BOSSSTART+1+SPACECOLOFF
    sta BOSSSTART+2+SPACECOLOFF
    sta BOSSSTART+3+SPACECOLOFF
    
    sta BOSSSTART+ROWDIFF+SPACECOLOFF
    sta BOSSSTART+1+ROWDIFF+SPACECOLOFF
    sta BOSSSTART+2+ROWDIFF+SPACECOLOFF
    sta BOSSSTART+3+ROWDIFF+SPACECOLOFF
    
    sta BOSSSTART+2*ROWDIFF+SPACECOLOFF
    sta BOSSSTART+1+2*ROWDIFF+SPACECOLOFF
    sta BOSSSTART+2+2*ROWDIFF+SPACECOLOFF
    sta BOSSSTART+3+2*ROWDIFF+SPACECOLOFF
    
    sta BOSSSTART+3*ROWDIFF+SPACECOLOFF
    sta BOSSSTART+1+3*ROWDIFF+SPACECOLOFF
    sta BOSSSTART+2+3*ROWDIFF+SPACECOLOFF
    sta BOSSSTART+3+3*ROWDIFF+SPACECOLOFF
 

    ; Platforms
    lda #29
    sta 8036

    sta 8044
    ;sta 7994
        
    pla     ; reload x and acc
    tax
    pla
    
    rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Print Lives Subroutine                     ;
;--------------------------------------------;
; Prints Cuphead's current number of lives   ;
; Updates whenever Cuphead gets hit          ;
; (When life reaches 0, jump to lose screen) ;
; Args: None (get infro from cuphead lives)  ;
; Returns: Nothing                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

printlives
    ; lives
	ldx CHLIVES
	cpx #0
	bne drawhearts
	lda #3 ;cyan
    sta $1e17+SPACECOLOFF   
	jsr diswindead
	rts

drawhearts	
    ; char
    lda #27  ; heart symbol
    sta $1e17  ; heart #1
    sta $1e18 ; heart #2
    sta $1e19 ; heart #3
	
    ldy #2   ; red
	lda #3	 ; cyan 

    sty $1e17+SPACECOLOFF   
	
	cpx #2
	bmi oneheart
    sty $1e18+SPACECOLOFF
	cpx #3
	bmi twoheart
    sty $1e19+SPACECOLOFF
	rts
	
oneheart
    sta $1e18+SPACECOLOFF
	rts
twoheart
    sta $1e19+SPACECOLOFF 
	rts	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRINTFLOOR SUBROUTINE                     ;
; Arg: level of floor to printfloor; in X   ;
; returns: nothing                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printfloor
    pha
    txa
    pha

    ;lda #$a2        ; Floor
    cpx #0
    bne otherld
    lda #30
    jmp startfloor
otherld    
    lda #11
startfloor
    sta $1fa2,X
    sta $1fa3,X
    sta $1fa4,X
    sta $1fa5,X
    sta $1fa6,X
    
    sta $1fa7,X
    sta $1fa8,X
    sta $1fa9,X
    sta $1faa,X
    sta $1fab,X
    
    sta $1fac,X
    sta $1fad,X
    sta $1fae,X
    sta $1faf,X
    sta $1fb0,X
    
    sta $1fb1,X
    sta $1fb2,X
    sta $1fb3,X
    sta $1fb4,X
    sta $1fb5,X
    
    sta $1fb6,X
    sta $1fb7,X
    
    ;color floor
    lda #5
    sta $97a2,X
    sta $97a3,X
    sta $97a4,X
    sta $97a5,X
    sta $97a6,X
    
    sta $97a7,X
    sta $97a8,X
    sta $97a9,X
    sta $97aa,X
    sta $97ab,X
    
    sta $97ac,X
    sta $97ad,X
    sta $97ae,X
    sta $97af,X
    sta $97b0,X
    
    sta $97b1,X
    sta $97b2,X
    sta $97b3,X
    sta $97b4,X
    sta $97b5,X
    
    sta $97b6,X
    sta $97b7,X

    pla
    tax
    pla
    
    rts    
    
;;;;;;;;;;;;;;;;;;;
; WAIT2 SUBROUTINE;
;;;;;;;;;;;;;;;;;;;
wait2 
    pha     ; save registers
    txa
    pha 
    tya 
    pha
    
    ldx #$ff
 
wait2loop   
    dex
    beq wait2end   ; return now that loop is done
    jmp wait2loop 
    
    
wait2end
    pla     ; load registers
    tay
    pla
    tax
    pla

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SONG SUBROUTINE                    ;
;------------------------------------;
; plays the cuphead title theme song ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
song
	lda	#$0f
	sta	$900e	; set speaker volume to max 
	
	; 900b = speaker 2
	; 900c = speaker 3
	; quarter note = 250 (fa) / eighth note = 125 (7d) / half note = 1000 (3e8
	jsr	playa
	jsr	playa
	jsr playbf
	jsr	pause
	jsr playbf
	jsr	pause
	jsr playbf
	jsr playbf
	jsr	playg
	jsr	playg
	jsr	playf
	jsr	pause
	jsr	playf
	jsr	pause
	jsr	playf
	jsr	playf
	jsr	playa
	jsr	playa
	jsr playbf
	jsr	pause
	jsr playbf
	jsr	pause
	jsr playc
	jsr playc
	jsr	pause
	jsr playc
	jsr playc
	jsr	pause
	jsr playb
	jsr playb
	jsr playb
	jsr playb
	
	lda	#$0
	sta	$900e
	sta	$900c
	rts			; go back to main 

playa
	lda	#$da	; note a (218)
	sta	$900c	; play in speaker 2 )
	ldy	#$fa	; duration 
	jsr	play
	rts
	
playbf	
	lda	#$dc	; note b flat (220)
	sta	$900c	; play in speaker 2 )
	ldy	#$7d	; duration 
	jsr	play	
	rts

playb
	lda	#$de	; note b flat (220)
	sta	$900c	; play in speaker 2 )
	ldy	#$7d	; duration 
	jsr	play	
	rts
	
playc
	lda	#$e0	; note b flat (224)
	sta	$900c	; play in speaker 2 )
	ldy	#$7d	; duration 
	jsr	play	
	rts
	
	
playf
	lda	#$d0	; note a (214)
	sta	$900c	; play in speaker 2 )
	ldy	#$fa	; duration 
	jsr	play
	rts
	
playg
	lda	#$d6	; note a (214)
	sta	$900c	; play in speaker 2 )
	ldy	#$fa	; duration 
	jsr	play
	rts

playgf
	lda	#$d3	; note a (214)
	sta	$900c	; play in speaker 2 )
	ldy	#$fa	; duration 
	jsr	play
	rts	
	
pause
	lda	#$0
	sta	$900c
	ldy	#$7f	; duration 
    ;jsr nothing
    ;jsr nothing
	jsr	play
	rts
; plays note
play
	;lda	#$20
    ;jsr 
    jsr nothing
    ;jsr nothing
    ;jsr	$ffd2	; print if right if pressed
	dey
	bne	play

	rts
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; disstartscreen SUBROUTINE                                    ;
;--------------------------------------------------------------;
; Displays the start screen that features cuphead and his name ;
; Args: none                                                   ;
; Returns: nothing                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
disstartscreen
    pha

    jsr clear         ; clear screen    
    
    lda #40          ; change to red with black border
    sta $900f
    
    lda #255          ; change where it gets its characters from
    sta $9005

    ; Print spaces everywhere
    lda #12
    
    ldx #255
printspaces
    sta 7679,X
    dex
    bne printspaces
    
    ldx #251
printspaces2
    sta 7934,X
    dex
    bne printspaces2

    ; Display Cuphead Figure
    lda #0
    sta CUPHEADSTART
    
    lda #1
    sta CUPHEADSTART+ROWDIFF
    
    lda #2
    sta CUPHEADSTART+2*ROWDIFF
    
    lda #3
    sta CUPHEADSTART+3*ROWDIFF
    
    lda #4
    sta CUPHEADSTART+1
    
    lda #5
    sta CUPHEADSTART+1+ROWDIFF
    
    lda #6
    sta CUPHEADSTART+1+2*ROWDIFF
    
    lda #7
    sta CUPHEADSTART+1+3*ROWDIFF
    
    lda #8
    sta CUPHEADSTART+2
    
    lda #9
    sta CUPHEADSTART+2+ROWDIFF
    
    lda #10
    sta CUPHEADSTART+2+3*ROWDIFF

    ; Display Cuphead Word
    
    lda #11
    
    ; C
    sta CUPSTART
    sta CUPSTART+1
    sta CUPSTART+2
    
    sta CUPSTART+ROWDIFF
    sta CUPSTART+2*ROWDIFF
    sta CUPSTART+3*ROWDIFF
    
    sta CUPSTART+4*ROWDIFF
    sta CUPSTART+4*ROWDIFF+1
    sta CUPSTART+4*ROWDIFF+2
    
    ; U
    sta CUPSTART+4
    sta CUPSTART+6
    
    sta CUPSTART+4+ROWDIFF
    sta CUPSTART+6+ROWDIFF
    
    sta CUPSTART+4+2*ROWDIFF
    sta CUPSTART+6+2*ROWDIFF 
    
    sta CUPSTART+4+3*ROWDIFF
    sta CUPSTART+6+3*ROWDIFF 
    
    sta CUPSTART+4+4*ROWDIFF
    sta CUPSTART+5+4*ROWDIFF
    sta CUPSTART+6+4*ROWDIFF 
    
    ; P
    sta CUPSTART+8
    sta CUPSTART+9
    sta CUPSTART+10
    
    sta CUPSTART+8+ROWDIFF
    sta CUPSTART+10+ROWDIFF
    
    sta CUPSTART+8+2*ROWDIFF
    sta CUPSTART+9+2*ROWDIFF
    sta CUPSTART+10+2*ROWDIFF
    
    sta CUPSTART+8+3*ROWDIFF
    
    sta CUPSTART+8+4*ROWDIFF
    
    ; H
    sta HEADSTART
    sta HEADSTART+2
    
    sta HEADSTART+ROWDIFF
    sta HEADSTART+ROWDIFF+2
    
    sta HEADSTART+2*ROWDIFF
    sta HEADSTART+2*ROWDIFF+1
    sta HEADSTART+2*ROWDIFF+2
    
    sta HEADSTART+3*ROWDIFF
    sta HEADSTART+3*ROWDIFF+2
    
    sta HEADSTART+4*ROWDIFF
    sta HEADSTART+4*ROWDIFF+2
    
    ;E
    sta HEADSTART+4
    sta HEADSTART+5
    sta HEADSTART+6
    
    sta HEADSTART+4+ROWDIFF
    
    sta HEADSTART+4+2*ROWDIFF
    sta HEADSTART+4+2*ROWDIFF+1
    sta HEADSTART+4+2*ROWDIFF+2
    
    sta HEADSTART+4+3*ROWDIFF
    
    sta HEADSTART+4+4*ROWDIFF
    sta HEADSTART+4+4*ROWDIFF+1
    sta HEADSTART+4+4*ROWDIFF+2
    
    ; A
    sta HEADSTART+8
    sta HEADSTART+9
    sta HEADSTART+10
    
    sta HEADSTART+8+ROWDIFF
    sta HEADSTART+8+ROWDIFF+2
    
    sta HEADSTART+8+2*ROWDIFF
    sta HEADSTART+8+2*ROWDIFF+1
    sta HEADSTART+8+2*ROWDIFF+2
    
    sta HEADSTART+8+3*ROWDIFF
    sta HEADSTART+8+3*ROWDIFF+2
    
    sta HEADSTART+8+4*ROWDIFF
    sta HEADSTART+8+4*ROWDIFF+2
    
    ;D
    sta HEADSTART+12
    sta HEADSTART+13
    
    sta HEADSTART+12+ROWDIFF
    sta HEADSTART+12+ROWDIFF+2
    
    sta HEADSTART+12+2*ROWDIFF
    sta HEADSTART+12+2*ROWDIFF+2
    
    sta HEADSTART+12+3*ROWDIFF
    sta HEADSTART+12+3*ROWDIFF+2
    
    sta HEADSTART+12+4*ROWDIFF
    sta HEADSTART+12+4*ROWDIFF+1
    
    pla
    
    rts
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display Title Screen Options    ;
;---------------------------------;
; Args: None                      ;
; Returns: Nothing                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
disoptions
    pha
    txa
    pha
    tya
    pha
    
    ;;; Erase Cuphead ;;;
    lda #12
    sta CUPHEADSTART
    sta CUPHEADSTART+ROWDIFF
    sta CUPHEADSTART+2*ROWDIFF
    sta CUPHEADSTART+3*ROWDIFF
    sta CUPHEADSTART+1
    sta CUPHEADSTART+1+ROWDIFF
    sta CUPHEADSTART+1+2*ROWDIFF
    sta CUPHEADSTART+1+3*ROWDIFF
    sta CUPHEADSTART+2
    sta CUPHEADSTART+2+ROWDIFF
    sta CUPHEADSTART+2+3*ROWDIFF
    
    
    ;;; Redraw Cuphead to the left
    lda #0
    sta CUPHEADSTART-6-ROWDIFF
    
    lda #1
    sta CUPHEADSTART+ROWDIFF-6-ROWDIFF
    
    lda #2
    sta CUPHEADSTART+2*ROWDIFF-6-ROWDIFF
    
    lda #3
    sta CUPHEADSTART+3*ROWDIFF-6-ROWDIFF
    
    lda #4
    sta CUPHEADSTART+1-6-ROWDIFF
    
    lda #5
    sta CUPHEADSTART+1+ROWDIFF-6-ROWDIFF
    
    lda #6
    sta CUPHEADSTART+1+2*ROWDIFF-6-ROWDIFF
    
    lda #7
    sta CUPHEADSTART+1+3*ROWDIFF-6-ROWDIFF
    
    lda #8
    sta CUPHEADSTART+2-6-ROWDIFF
    
    lda #9
    sta CUPHEADSTART+2+ROWDIFF-6-ROWDIFF
    
    lda #10
    sta CUPHEADSTART+2+3*ROWDIFF-6-ROWDIFF
    
    ;;;;;;; Display "PLAY" ;;;;;;;
    ;P
    lda #13
    sta OPTIONSTART
    
    ;L
    lda #14
    sta OPTIONSTART+1
    
    ;A
    lda #15
    sta OPTIONSTART+2
    
    ;Y
    lda #16
    sta OPTIONSTART+3
    
    ;;;;;;Display "CREDITS";;;;;;;;;
    ;C
    lda #17
    sta OPTIONSTART+2*ROWDIFF
    
    ;R
    lda #18
    sta OPTIONSTART+2*ROWDIFF+1
    
    ;E
    lda #19
    sta OPTIONSTART+2*ROWDIFF+2

    ;D
    lda #20
    sta OPTIONSTART+2*ROWDIFF+3
    
    ;I
    lda #21
    sta OPTIONSTART+44+4
    
    ;T
    lda #22
    sta OPTIONSTART+44+5
    
    ;S
    lda #23
    sta OPTIONSTART+44+6
    
    
    ;;;;; Display Start Arrow ;;;;;;;
    lda #26
    sta OPTIONSTART-1
    
    pla
    tay
    pla
    tax
    pla
    
    rts
 
;;;;;;;;;;;;;;;;;;;;;;;
; Display Credits     ;
;---------------------;
; Draw Devs' Initials ;
;                     ;
; Args: None          ;
; Returns: Nothing    ;
;;;;;;;;;;;;;;;;;;;;;;;
discredits
    pha
    
    ; MM
    lda #25
    sta CREDITSTART
    sta CREDITSTART+1
    
    ; NL
    lda #24
    sta CREDITSTART+3
    lda #14
    sta CREDITSTART+4
    
    ; SS
    lda #23
    sta CREDITSTART+6
    sta CREDITSTART+7
    
    pla
    
    rts


;;Time waster    
nothing
    pha
    txa
    pha

    ldx #$99
nothingloop   
    dex
    bne nothingloop
    
    ldx #$87
nothingloop2    
    dex
    bne nothingloop2

nothingend    
    pla
    tax
    pla
    rts
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boss Shoot Check SUBROUTINE ;
;-----------------------------;
; Determine if boss can shoot ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
boss_shoot_check  
    ;lda
    
    dec TIMERCOUNT1
    lda TIMERCOUNT1
    and $ff
    bne bscend
    
    dec TIMERCOUNT2
    lda TIMERCOUNT2
    and $ff
    bne bscend
    
    inc TIMERCOUNT1
    lda TIMERCOUNT1
    sbc #255
    bmi bscend
    
    ;jsr boss_shoot
    
    ; Check if the previous shot ended
    lda BSHOOT
    bne skipshootboss   
    
    ; Shoot sound effect
    ; PLAY SHOOT SOUND EFFECT
    lda #$0f	; vol 15
	sta $900e	; store in vol mem (36878)
    ; Loop and decrement 254 to 128 
    lda #$80    ; load 128 into acc
    sta 7165  ; store at mem loc 7165; used for comparison later
    ; put 254 in x
    ldx #$fe
    
bossshootloop
    stx $900a  ; put in third speaker
    ldy #$ff   ; make note last longer
bossshoottimer   ; make the notes last a little longer
    dey
    bne bossshoottimer
    sty $900a   ; store 0 in third speaker after
    iny
    dex         ; dec x by 1    
    txa         ; for check
    sbc 7165   ; check if at 128
    bne bossshootloop
    inx
    txa
    
    ; load BSHOOT
    ;1yyxxxxx
    ;lda #0
    ;sta BST1
    ;sta BST2
    lda $1
    asl
    asl
    asl
    asl
    asl
    
    clc
    adc #$80
    
    clc
    adc #$10
    
    ;adc $0
    ;clc
    ;sbc #1
    ;lda #$90
    sta BSHOOT

skipshootboss    
    ; reset for next round
    lda #$99
    sta TIMERCOUNT1   
    sta TIMERCOUNT2   

bscend
    rts
    

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Win/Dead Screen Display                  ;
;------------------------------------------;
; Args: None (get info from cuphead lives) ;
; Returns: Nothing                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
diswindead
    sei

    ldy #16
    lda #11
spaceloop
    cpy #0
    beq cupprint
    
    sta MSGSTART,Y  ; Print white block here
    sta MSGSTART+176,Y  
    dey
    jmp spaceloop  
    
cupprint    
    ; Display Cuphead Figure
    lda #0
    sta CUPMSGSTART
    
    lda #1
    sta CUPMSGSTART+ROWDIFF
    
    lda #2
    sta CUPMSGSTART+2*ROWDIFF
    
    lda #3
    sta CUPMSGSTART+3*ROWDIFF
    
    lda #4
    sta CUPMSGSTART+1
    
    lda #5
    sta CUPMSGSTART+1+ROWDIFF
    
    lda #6
    sta CUPMSGSTART+1+2*ROWDIFF
    
    lda #7
    sta CUPMSGSTART+1+3*ROWDIFF
    
    lda #8
    sta CUPMSGSTART+2
    
    lda #9
    sta CUPMSGSTART+2+ROWDIFF
    
    lda #10
    sta CUPMSGSTART+2+3*ROWDIFF

    ; If 0, display Lose
    lda CHLIVES
    beq deadword
    
    ; Otherwise, show win 
    ; Win Sound Effect
    lda #240    ; G
    sta $900c
    jsr wait
    jsr wait
    lda #0
    sta $900c
    
    ; Display YAY
    lda #16  ;Y
    sta WORDSTART
    sta WORDSTART+2
    lda #15  ;A
    sta WORDSTART+1
    
infinite    
    jmp infinite
    
    
deadword 
    ; Lose Sound effect
    lda #135    ; G
    sta $900a
    jsr wait
    jsr wait
    lda #0
    sta $900a
    
    ; Display DEAD
    lda #20
    sta WORDSTART
    sta WORDSTART+3
    lda #19
    sta WORDSTART+1
    lda #15
    sta WORDSTART+2

selectwait   
    jsr wait
    jsr wait
    lda 197                                 ; current key pressed

    cmp #32                                 ; space		
    beq backtobegin

    jmp selectwait 
backtobegin
    jsr main

    rts
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display Tombstone Subroutine ;
;------------------------------;
; Displays the Tombstone Boss  ;
; Args: none                   ;
; Returns: nothing             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

distombstone
    pha
    
    lda #49
    sta TOMBSTART
    lda #50
    sta TOMBSTART+1
    lda #51
    sta TOMBSTART+2
    
    lda #52
    sta TOMBSTART+ROWDIFF
    lda #53
    sta TOMBSTART+ROWDIFF+1
    lda #54
    sta TOMBSTART+ROWDIFF+2
    
    lda #55
    sta TOMBSTART+2*ROWDIFF
    lda #56
    sta TOMBSTART+2*ROWDIFF+1
    lda #57
    sta TOMBSTART+2*ROWDIFF+2
       
    lda #58
    sta TOMBSTART+3*ROWDIFF
    lda #59
    sta TOMBSTART+3*ROWDIFF+1
    lda #60
    sta TOMBSTART+3*ROWDIFF+2
        
    ; Color - Black
    lda #0
    sta TOMBSTART+SPACECOLOFF
    sta TOMBSTART+1+SPACECOLOFF
    sta TOMBSTART+2+SPACECOLOFF
    
    sta TOMBSTART+ROWDIFF+SPACECOLOFF
    sta TOMBSTART+ROWDIFF+1+SPACECOLOFF
    sta TOMBSTART+ROWDIFF+2+SPACECOLOFF
    
    sta TOMBSTART+2*ROWDIFF+SPACECOLOFF
    sta TOMBSTART+2*ROWDIFF+1+SPACECOLOFF
    sta TOMBSTART+2*ROWDIFF+2+SPACECOLOFF
       
    sta TOMBSTART+3*ROWDIFF+SPACECOLOFF
    sta TOMBSTART+3*ROWDIFF+1+SPACECOLOFF
    sta TOMBSTART+3*ROWDIFF+2+SPACECOLOFF
        
    ; Erase Blueberry
    lda #12
    sta TOMBSTART+3
    sta TOMBSTART+ROWDIFF+3
    sta TOMBSTART+2*ROWDIFF+3
    sta TOMBSTART+3*ROWDIFF+3    
        
    pla

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
; Placeholder Cuphead for jump  ;
;-------------------------------;
; displays cuphead when on jump ;
; Args: none                    ;
; Returns: nothing              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

chplaceholder   
    ; placeholder cuphead
    lda #31
    sta 8014
    lda #2      ; store color of cuphead
    sta 8014+SPACECOLOFF
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
    sta CUPYOFFSET,X  ; CUPYOFFSET + x -(y*22)
    lda #2    ;red
    sta CUPYOFFSET+SPACECOLOFF,X
    
    ; Erase previous bullet 
    lda #12
    sta CUPYOFFSET-1,X  ; CUPYOFFSET + X -(Y*22)-1

    inc CHSHOOT    ; next location
       
    
    ; Check if end of shot; reset bit 0 of CHSHOOT
    lda CHSHOOT
    and #$1f
    cmp #$12
    ;cmp #     ;BOSSPOSI+4
    ;bmi chkboss  ; if not at end, just move on to if boss shoots
    bne crsttime 
    lda #0     ; otherwise, clear shoot bit
    sta CHSHOOT
    lda #12     ; also erase last bullet
    sta CUPYOFFSET,X
    ; Collision resolution  - must have hit boss since he doesn't move
    dec BOSSLIVES
    
    
crsttime    
    ; Reset timer if not at end 
    ;lda #0
    ;sta CHST1
    ;lda #0
    ;sta CHST2 
    
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
    

    
    
    ; Collision resolution first here
    ; if location of bullet == cuphead's location, decrease cuphead's lives
    ; Check x 
    lda BSHOOT
    and #$1f
    clc
    sbc #1
    cmp $0
    bne wallchk
    
    ;Check y
    lda BSHOOT
    and #$60
    cmp $1
    bne wallchk
    
    dec CHLIVES		; update cuphead's life when hit
	jsr printlives
    ; Also reset at this point and stop shooting
    
    lda #0       ; clear shoot bit
    sta BSHOOT
    
    
    lda #12     ; also erase last bullet
    ldx $0
    sta CUPYOFFSET+1,X   
    
    jmp brsttime
    
wallchk
    dec BSHOOT    ; next location
 
    ; check if wall reached Check if end of shot; reset bit 0 of BSHOOT   
    lda BSHOOT
    and #$1f
    ;cmp #     ;BOSSPOSI+4
    ;bmi chkboss  ; if not at end, just move on to if boss shoots
    bne brsttime
    lda #0       ; otherwise, clear shoot bit
    sta BSHOOT
    lda #12     ; also erase last bullet
    sta CUPYOFFSET-1   ;CHANGE AFTER TESTING!!!!!!
        
brsttime   
    ; Reset timer if not at end 
    ;lda #$3
    ;sta BST1
    ;lda #0
    ;sta BST2 
    
    pla
    tax
    pla
   
    rts
    
;;;;;;;;;;;;;;;;;;;;;;;;
; Timer ISR            ;
;----------------------;
; -Cuphead Shooting    ;
; -Boss Shooting       ;
; ...                  ;
;;;;;;;;;;;;;;;;;;;;;;;;
timer_isr
    pha

    lda $911d   ; check interupt flags
    and #$20
    beq return
    
    lda $9118    ; read from low order to reset
    lda $9119
    lda #0
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
    ;lda CHST1
    ;beq chst2chk  ; if equal, check next timer
    ;dec CHST1     ; if not equal, decrement timer and just move on  
    ;jmp chkboss
    
;chst2chk    
    ;lda CHST2
    ;beq cisshoot  ; if 0, good to shoot
    ;dec CHST2     ; if not equal, decrement timer and just move on  
    ;jmp chkboss    
 
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
    ;lda BST1
    ;beq bst2chk
    ;dec BST1
    ;jmp musicnote

;bst2chk
    ;lda BST2
    ;beq bisshoot
    ;dec BST2
    ;jmp musicnote
    
    
;bisshoot 
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

    ; set timer
    ;lda #$ff     
    ;sta $9119     
    ;sta $9118   
    
    ; set timer 2
    lda #$07     ; 2s 
    sta $9119
    lda #$d0     ; 2s 
    sta $9118        

return  
    pla
    jmp $eabf
    ;rti
    ;jmp  $fead




    
    
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
