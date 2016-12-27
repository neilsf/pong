
    ; The PONG GAME

    *=$0800

    ; setup screen

			lda #$00
			sta $d020
			sta $d021
    
	clear		lda #$20
			sta $0400,x
			sta $0500,x 
			sta $0600,x 
			sta $06e8,x 
			lda #$0f
			sta $d800,x  
                	sta $d900,x
                	sta $da00,x
                	sta $dae8,x
                	inx
                	bne clear

    ; welcome screen

			ldx #39
			lda #$f9
		l1	sta $0400,x
			dex
			bpl l1

			ldx #39
			lda #$f8
		l2	sta $07c0,x
			dex
			bpl l2

			lda #$3b
			sta $fb
			lda #$04
			sta $fc


			ldy #$00
			
			ldx #12
		l3	lda #$76
			sta ($fb),y
			iny
			lda #$75
			sta ($fb),y
			dey

			lda $fb
			clc
			adc #80

			bcc noinc
			inc $fc
		noinc	sta $fb
			dex
			bne l3

			rts

    ; game loop

    ; Sprites
    ;
    ; All sprites monochrome, 21x3 bytes

    player_left		.repeat 21, $ff,$00,$00

    player_right	.repeat 21, $00,$00,$ff

    ball		.repeat 8,  $00,$ff,$00
			.repeat 13, $00,$00,$00
