; draw basic playfield
; testing drawing something meaniningful with VIC Characters
SPACECOLOFF EQU $7800  ; difference between location in space and it's color location
CUPYOFFSET EQU 8076
ROWDIFF EQU 22

; For drawing start screen
CUPHEADSTART EQU 8064
CUPSTART EQU 7751
HEADSTART EQU 7881
OPTIONSTART EQU 8063

; Boss Timer 2 Info - Do not use for anything else!
TIMERCOUNT1 EQU 7167
TIMERCOUNT2 EQU 7166

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
	jsr	song		; play the title song
    
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
    ;jmp loop

    jmp optionchoiceloop
    
upperop    
    ; If at play, don't move up
    txa
    cmp #0
    beq optionchoiceloop
    
    ; If at instruction, move up
    cmp #1
    bne uo2
    ; Delete previous arrow
    lda #12     
    sta OPTIONSTART-1
    ; Draw New Arrow
    lda #27
    sta OPTIONSTART-2*ROWDIFF-1
    
    dex
    jmp optionchoiceloop
uo2    
    ; If at credits, move up
    ; Delete previous arrow
    lda #12
    sta OPTIONSTART+2*ROWDIFF-1
    ; Draw New Arrow
    lda #27
    sta OPTIONSTART-1
    
    dex
    jmp optionchoiceloop

lowerop
    ; If at credits, don't move down
    txa
    cmp #2
    beq optionchoiceloop
    
    ; If at play, move down
    cmp #0
    bne lo1
    ; Delete Previous Arrow
    lda #12
    sta OPTIONSTART-2*ROWDIFF-1
    ; Draw New Arrow
    lda #27
    sta OPTIONSTART-1
    
    inx
    jmp optionchoiceloop
lo1 ; Otherwise, at Instructions   
    ; Delete previous arrow
    lda #12     
    sta OPTIONSTART-1
    
    ; Draw New Arrow
    lda #27
    sta OPTIONSTART+2*ROWDIFF-1
    
    inx
    jmp optionchoiceloop
    
select
    ; Just play for now; no other options
    
    jsr wait
    jsr wait
    
    ;lda #8           ; change to black
    lda #184          ; change to light cyan
    sta $900f
    
    lda #240        ; change back to where it originally got its characters
    sta $9005
    
    ; set up for boss check
    lda #$99
    sta TIMERCOUNT1   
    
    lda #$99
    sta TIMERCOUNT2
    
    jsr clear

    jsr playfield
    jsr redpath
   
    ; store box at starting position 8076
    lda #81               
    sta 8076
        
        ; start at position 0,0
         ldx #0
         stx $0     ; x coord                            
         stx $1     ; y coord
    
loop    ; Check if boss shoots
        jsr boss_shoot_check
        
        jsr wait
        lda 197                                 ; current key pressed
        cmp #9                                  ; w
        beq up                                  ; up
        cmp #17                                 ; a
        beq left                                ; left
        cmp #41                                 ; s
        beq down                                ; down
        cmp #18                                 ; d
        beq right                               ; right
        
        ; check if pressed shoot button
        cmp #32                                 ; space		
        beq shoot
        jmp loop
        
        

endloop 
        ;jsr playfield
        ldx $0
        lda #96
        sta CUPYOFFSET-1,X   
        sta CUPYOFFSET+1,X          
        
        jsr draw
        jmp loop
        
up      ldx $1
        dex                                     ; move up 1
        txa
        cmp #$ff                                ; boundaries
        beq endloop
        ;stx $1     ;commented out so don't move up
        jmp endloop

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
        cmp #$e
        beq endloop
        stx $0
        jmp endloop


down    ldx $1      ;
        inx
        txa
        cmp #$10    ; stop at floor
        beq endloop
        ; stx $1    ;commented out so don't move down
        jmp endloop 

;;;;;;;;;;;;;;;;;;;
;SHOOT SUBROUTINE ;
; args: none      ;
; returns: nothing;
;;;;;;;;;;;;;;;;;;;
shoot       
    pha     ; save registers
    txa
    pha 
    tya 
    pha
    
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
    
    ;DRAW BULLET
    ldx $0 ; load y coordinate
    ;txa
    ;clc
    ;adc #0 ; increment to next
    ;adc CUPYOFFSET ; add offset
    ;tax
    lda #45
    sta CUPYOFFSET+1,X
    lda #2  ;make bullet red
    sta SPACECOLOFF+CUPYOFFSET+1,X

bulletloop    
    inx            ; reg X = y location of player without screen offset
    txa
    sbc #13
    beq shootend
    txa 
    sbc #14        ; past boss
    bpl shootend
    jsr wait2      ; pause for a bit
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2 
    jsr wait2      ; pause for a bit
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2     
    lda #96         ;erase previous bullet with space
    sta CUPYOFFSET,X
    lda #45         ; add next bullet in line
    sta CUPYOFFSET+1,X
    lda #2  ;make bullet red
    sta SPACECOLOFF+CUPYOFFSET+1,X
    jmp bulletloop
    
shootend
    jsr wait2      ; pause for a bit
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2 
    jsr wait2      ; pause for a bit
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2 

    lda #96         ;erase last bullet
    sta $1f9a

    pla     ; load registers
    tay
    pla
    tax
    pla

    jmp loop

 
;;;;;;;;;;;;;;;;;;;;;;
; CUPHEAD SUBROUTINE ;
;;;;;;;;;;;;;;;;;;;;;;
cuphead
    pha
    txa
    pha 

    ldx #81        ; cuphead
    stx 8076
    
    pla
    tax
    pla
    
    rts

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


draw1   ;jsr clear
        lda #81
        sta 8076,Y
        
        ; add color
        lda #2
        sta 8076+SPACECOLOFF,Y
        
        jmp enddraw
        
draw2   ;jsr clear
        lda #$81
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
    
    ldx #0
    jsr printfloor
    ldx #ROWDIFF
    jsr printfloor
    ldx #ROWDIFF*2
    jsr printfloor
    ldx #ROWDIFF*3
    jsr printfloor

     
printlives
    ; lives
    ; char
    lda #83        
    sta $1e17    
    sta $1e18
    sta $1e19
    ;color
    lda #2
    sta $1e17+SPACECOLOFF    
    sta $1e18+SPACECOLOFF
    sta $1e19+SPACECOLOFF
    
    
    ;boss
    ;corners
    lda #122    ;bottom right
    sta $1fa0
    lda #6
    sta $1fa0+SPACECOLOFF
    
    lda #76     ;bottom left
    sta $1f9b
    lda #6
    sta $1f9b+SPACECOLOFF
    
    lda #79     ;upper left
    sta $1f17
    lda #6
    sta $1f17+SPACECOLOFF
    
    lda #80     ;upper right
    sta $1f1c
    lda #6
    sta $1f1c+SPACECOLOFF
      
    ;bottom
    lda #100
    sta $1f9c
    sta $1f9d
    sta $1f9e
    sta $1f9f
    ;color
    lda #6
    sta $1f9c+SPACECOLOFF
    sta $1f9d+SPACECOLOFF
    sta $1f9e+SPACECOLOFF
    sta $1f9f+SPACECOLOFF
    
    ;top
    lda #99
    sta $1f18
    sta $1f19
    sta $1f1a
    sta $1f1b
    ;color
    lda #6
    sta $1f18+SPACECOLOFF
    sta $1f19+SPACECOLOFF
    sta $1f1a+SPACECOLOFF
    sta $1f1b+SPACECOLOFF
    
    ;left
    lda #101
    sta $1f85
    sta $1f6f
    sta $1f59
    sta $1f43
    sta $1f2d
    ;color
    lda #6
    sta $1f85+SPACECOLOFF
    sta $1f6f+SPACECOLOFF
    sta $1f59+SPACECOLOFF
    sta $1f43+SPACECOLOFF
    sta $1f2d+SPACECOLOFF
    
    ;lda #81
    lda #103
    sta $1f8a
    sta $1f74
    sta $1f5e
    sta $1f48
    sta $1f32
    ;color
    lda #6
    sta $1f8a+SPACECOLOFF
    sta $1f74+SPACECOLOFF
    sta $1f5e+SPACECOLOFF
    sta $1f48+SPACECOLOFF
    sta $1f32+SPACECOLOFF   
    
    ;eyes
    lda #81
    sta $1f44
    sta $1f47
    ; color
    lda #0
    sta $1f44+SPACECOLOFF 
    sta $1f47+SPACECOLOFF 
    
    pla     ; reload x and acc
    tax
    pla
    
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
    lda #102
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
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RED PATH                                           ;
; makes it so cuphead and the bullets are always red ;    
; Arg: none                                          ;
; Returns: Nothing                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
redpath
    pha
    txa 
    pha

    ldx #14
    lda #2   ; red
redpathloop
    sta $1f8c+SPACECOLOFF,X
    dex
    bpl redpathloop
    
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
    
    lda #40          ; change to yellow with black border
    sta $900f
    
    lda #255          ; change where it gets its characters from
    sta $9005
    
    ; Char0
    ;1
    lda #30          ; ---****-
    sta 7168
    ;2
    lda #255         ; ********
    sta 7169
    ;3
    ;lda #248        ; *********
    sta 7170
    ;4
    lda #231         ; ***--***
    sta 7171
    ;5
    lda #15          ;----****
    sta 7172
    ;6
    lda #127         ; -*******
    sta 7173
    ;7
    lda #255        ; ********
    sta 7174
    ;8
    lda #207             ; **--****
    sta 7175
    
    ; Char1
    ;1
    lda #207            ;**--****
    sta 7176  
    ;2
    lda #111             ; -**-****
    sta 7177
    ;3
    lda #127               ; -*******
    sta 7178
    ;4
    lda #31               ; ---*****
    sta 7179
    ;5
    lda #0          ;--------
    sta 7180
    ;6
    lda #7            ; -----***
    sta 7181
    ;7
    lda #15              ; ----****
    sta 7182
    ;8
    lda #29               ; ---***-*
    sta 7183
  
    ; Char 2
    ;1
    lda #25             ; ---**--*
    sta 7184
    ;2
    lda #31                 ; ---*****
    sta 7185
    ;3
    lda #15               ; ----****
    sta 7186
    ;4
    ;lda                     ; ----****
    sta 7187
    ;5
    lda #3            ; ------**
    sta 7188
    ;6
    lda #1             ;-------*
    sta 7189
    ;7
    ;lda #1             ;-------*
    sta 7190
    ;8
    lda #0              ;--------
    sta 7191   

    ; Char3
    ;1
    lda #0              ;--------
    sta 7192
    ;2
    lda #6                 ; -----**-
    sta 7193
    ;3
    lda #15                     ; ----****
    sta 7194
    ;4
    ;lda #15                     ; ----****
    sta 7195
    ;5,6,7,8
    lda #0
    sta 7196
    sta 7197
    sta 7198
    sta 7199
    
    ; Char4
    ;1,2,3,4
    lda #0                  ; --------
    sta 7200
    sta 7201
    sta 7202
    sta 7203
    ;5,6,7,8
    lda #255                ; *******
    sta 7204
    sta 7205
    sta 7206
    sta 7207
    
    ; Char5
    ;1,2,3
    lda #255                ; *******
    sta 7208
    sta 7209
    sta 7210
    ;4
    lda #254                       ; *******-
    sta 7211
    ;5
    lda #252                   ; ******--
    sta 7212
    ;6,7
    lda #255                    ; *******
    sta 7213
    sta 7214
    ;8
    lda #254                    ; *******-
    sta 7215
    
    ; Char6
    ;1,2
    lda #254             ; *******-
    sta 7216
    sta 7217 
    ;3,4,5
    lda #255            ; ********
    sta 7218
    sta 7219
    sta 7220
    ;6,7
    lda #239               ; ***-****
    sta 7221
    sta 7222
    ;8
    lda #198                     ; **---**-
    sta 7223
    
    ; Char7
    ;1,2
    lda #198                     ; **---**-
    sta 7224
    sta 7225
    ;3,4
    lda #199                    ; **---***
    sta 7226
    sta 7227
    ;5,6,7,8
    lda #0                      ;--------
    sta 7228
    sta 7229
    sta 7230
    sta 7231
    
    ; Char8
    ;1,2,3,4
    lda #0                      ;--------
    sta 7232
    sta 7233
    sta 7234
    sta 7235
    ;5,6,7,8
    lda #192                         ;**------
    sta 7236
    sta 7237
    sta 7238
    sta 7239
    
    ; Char9
    ;1
    lda #192                         ;**------
    sta 7240
    ;2
    lda #128                        ; *-------
    sta 7241
    ;3,4
    lda #8                            ;----*---
    sta 7242
    sta 7243
    ;5
    lda #31                        ; ---*****
    sta 7244
    ;6,7
    lda #252                           ; ******--
    sta 7245
    sta 7246
    ;8 
    lda #0                              ;--------
    sta 7247
    
    ; Char10
    ;1
    lda #0                              ;--------
    sta 7248
    ;2
    lda #192                                ;**------
    sta 7249
    ;3,4
    lda #224                                ;***-----
    sta 7250
    sta 7251
    ;5,6,7,8
    lda #0                                  ;--------
    sta 7252
    sta 7253
    sta 7254
    sta 7255
        
    ; Char 11 - Block
    ;1,8
    lda #255
    sta 7256
    sta 7257
    sta 7258
    sta 7259
    
    sta 7260
    sta 7261
    sta 7262
    sta 7263
    
    ; Char 12 - space
    ;1,8
    lda #0
    sta 7264
    sta 7265
    sta 7266
    sta 7267
    
    sta 7268
    sta 7269
    sta 7270
    sta 7271
    
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
    
    ;ldx #249


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
    
    ; Char 13 - Letter I
    lda #$0
    sta 7272

    lda #$3e
    sta 7273
    sta 7279
    
    lda #$8
    sta 7274
    sta 7275
    sta 7276
    sta 7277
    sta 7278
    
    ; Char 14 - Letter N
    lda #$0
    sta 7280
    
    lda #$42
    sta 7281
    sta 7286
    sta 7287
    
    lda #$62
    sta 7282
    
    lda #$52
    sta 7283
    
    lda #$4a
    sta 7284
    
    lda #$46
    sta 7285
    
    ; Char 15 = Letter S
    lda #$0
    sta 7288
    
    lda #$3c
    sta 7289
    sta 7292
    sta 7295
    
    lda #$42
    sta 7290
    sta 7294
    
    lda #$40
    sta 7291
    
    lda #$2
    sta 7293
    
    ; Char 16 = Letter T
    lda #$0
    sta 7296
    
    lda #$3e
    sta 7297
    
    lda #$8
    sta 7298
    sta 7299
    sta 7300
    sta 7301
    sta 7302
    sta 7303
    
    ; Char 17 = Letter R
    lda #$0
    sta 7304
    
    lda #$7c
    sta 7305
    
    lda #$42
    sta 7306
    sta 7307
    sta 7311
    
    lda #$7c
    sta 7308
    
    lda #$48
    sta 7309
    
    lda #$44
    sta 7310
    
    ; Char 18 = Letter U
    lda #$0
    sta 7312

    lda #$42
    sta 7313
    sta 7314
    sta 7315
    sta 7316
    sta 7317
    sta 7318

    lda #$3c
    sta 7319
    
    ; Char 19 = Letter C
    lda #$0
    sta 7320
    
    lda #$1c
    sta 7321
    sta 7327
    
    lda #$22
    sta 7322
    sta 7326
    
    lda #$40
    sta 7323
    sta 7324
    sta 7325
    
    ; Char 20 = Letter O
    lda #$0
    sta 7328
    
    lda #$18
    sta 7329
    
    lda #$24
    sta 7330
    
    lda #$42
    sta 7331
    sta 7332
    sta 7333
    
    lda #$24
    sta 7334
    
    lda #$18
    sta 7335
    
    ; Char 21 = Letter P
    lda #$0
    sta 7336
    
    lda #$7c
    sta 7337
    sta 7340
    
    lda #$42
    sta 7338
    sta 7339
    
    lda #$40
    sta 7341
    sta 7342
    sta 7343
    
    ; Char 22 = Letter L
    lda #$0
    sta 7344
    
    lda #$40
    sta 7345
    sta 7346
    sta 7347
    sta 7348
    sta 7349
    sta 7350

    lda #$7e
    sta 7351
    
    ; Char 23 = Letter A
    lda #$0
    sta 7352
    
    lda #$18
    sta 7353
    
    lda #$24
    sta 7354
    
    lda #$42
    sta 7355
    sta 7357
    sta 7358
    sta 7359
    
    lda #$7e
    sta 7356
    
    ; Char 24 = Letter Y
    lda #$0
    sta 7360
    
    lda #$22
    sta 7361
    sta 7362
    sta 7363
    
    lda #$1c
    sta 7364
    
    lda #$8
    sta 7365
    sta 7366
    sta 7367
    
    ; Char 25 = Letter E
    lda #$0
    sta 7368
    
    lda #$7e
    sta 7369
    sta 7375
    
    lda #$40
    sta 7370
    sta 7371
    sta 7373
    sta 7374
    
    lda #$7c
    sta 7372
    
    ;Char 26 = Letter D
    lda #$0
    sta 7376
    
    lda #$78
    sta 7377
    sta 7383
    
    lda #$24
    sta 7378
    sta 7382
    
    lda #$22
    sta 7379
    sta 7380
    sta 7381
    

    ; Char 27 = Arrow Char
    lda #$0
    sta 7384
    
    lda #$30
    sta 7385
    
    lda #$18
    sta 7386
    sta 7390
    
    lda #$c
    sta 7387
    sta 7389
    
    lda #$6
    sta 7388
    
    lda #$30
    sta 7391
    
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
    
    ;;; Display "INSTRUCTIONS";;;;
    ;I
    lda #13
    sta OPTIONSTART
    
    ;N
    lda #14
    sta OPTIONSTART+1
    
    ;S
    lda #15
    sta OPTIONSTART+2
    
    ;T
    lda #16
    sta OPTIONSTART+3
    
    ;R
    lda #17
    sta OPTIONSTART+4
    
    ;U
    lda #18
    sta OPTIONSTART+5
    
    ;C
    lda #19
    sta OPTIONSTART+6
    
    ;T
    lda #16
    sta OPTIONSTART+7
    
    ;I
    lda #13
    sta OPTIONSTART+8
    
    ;O
    lda #20
    sta OPTIONSTART+9
    
    ;N
    lda #14
    sta OPTIONSTART+10
    
    ;S
    lda #15
    sta OPTIONSTART+11
    
    ;;;;;;; Display "PLAY" ;;;;;;;
    ;P
    lda #21
    sta OPTIONSTART-2*ROWDIFF
    
    ;L
    lda #22
    sta OPTIONSTART-2*ROWDIFF+1
    
    ;A
    lda #23
    sta OPTIONSTART-2*ROWDIFF+2
    
    ;Y
    lda #24
    sta OPTIONSTART-2*ROWDIFF+3
    
    ;;;;;;Display "CREDITS";;;;;;;;;
    ;C
    lda #19
    sta OPTIONSTART+2*ROWDIFF
    
    ;R
    lda #17
    sta OPTIONSTART+2*ROWDIFF+1
    
    ;E
    lda #25
    sta OPTIONSTART+2*ROWDIFF+2

    ;D
    lda #26
    sta OPTIONSTART+2*ROWDIFF+3
    
    ;I
    lda #13
    sta OPTIONSTART+44+4
    
    ;T
    lda #16
    sta OPTIONSTART+44+5
    
    ;S
    lda #15
    sta OPTIONSTART+44+6
    
    
    ;;;;; Display Start Arrow ;;;;;;;
    lda #27
    sta OPTIONSTART-2*ROWDIFF-1
    
    pla
    tay
    pla
    tax
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
    
    ;dec 7166
    ;lda 7166
    ;and $ff
    ;bne bscend
    
    ;lda #$00ce
    ;and $ff
    ;bne bscend
    
    jsr boss_shoot
    
    ; reset for next round
    lda #$99
    sta TIMERCOUNT1   
    
    ;lda #$99
    sta TIMERCOUNT2   

bscend
    rts
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BOSS SHOOT SUBROUTINE             ;
;-----------------------------------;
; Actually issues bullets from boss ;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
boss_shoot
    pha     ; save registers
    txa
    pha
    tya
    pha
    
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
    
    
    
    ;pla     ; load registers
    ;tay
    ;pla
    ;tax
    ;pla
    
    ;rts
    
    ; Draw first bullet
    lda #45
    sta CUPYOFFSET+14
    
    lda #6
    sta CUPYOFFSET+14+SPACECOLOFF
    
    ;ldy $0      ; get position of player
    ;pla     ; load registers
    ;tay
    ;pla
    ;tax
    ;pla
    
    ;rts
    
    ;DRAW BULLET
    ldx #14                ; start position of drawing bullet
    ;txa
    ;clc
    ;adc $1 ; increment to next
    ;adc CUPYOFFSET ; add offset
    ;tax
    ;lda #45
    ;sta CUPYOFFSET+1,X
    ;lda #6  ;make bullet red
    ;sta SPACECOLOFF+CUPYOFFSET+1,X

    ldy $1 ;load in y coordinate
bossbulletloop    
    dex            
    txa
    clc
    sbc #$0,Y    
    clc
    adc #1
    beq bossshootend
    ;txa
    ;sbc #14        ; past boss
    ;bpl bossshootend
    jsr wait2      ; pause for a bit
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2      ; pause for a bit
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2
    lda #96         ;erase previous bullet with space
    sta CUPYOFFSET+1,X
    lda #45         ; add next bullet in line
    sta CUPYOFFSET,X
    lda #6  ;make bullet blue
    sta SPACECOLOFF+CUPYOFFSET,X
    jmp bossbulletloop
    
bossshootend
    jsr wait2      ; pause for a bit
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2 
    jsr wait2      ; pause for a bit
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2
    jsr wait2 

    lda #96         ;erase last bullet
    ldy $1
    sta CUPYOFFSET+1,X

    pla     ; load registers
    tay
    pla
    tax
    pla
    
    rts
