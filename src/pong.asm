
	; The PONG GAME
	; https://github.com/neilsf/pong
	; feketecsaba@gmail.com

	*=$0801
	
	player_pos1	= $0334		; var char		Player 1 horizontal position
	player_pos2 = $0335		; var char		Player 2 horizontal position
	
	ball_posx	= $0336		; var fixed 	Exact X position of the ball
	ball_posy	= $033b		; var fixed 	Exact Y position of the ball
	
	ball_dx		= $0340		; var fixed 	Ball speed vector X
	ball_dy		= $0345		; var fixed 	Ball speed vector Y
	
	ball_posrx  = $034a		; var int 		X position of the ball rounded to 1 pixel
	ball_posry  = $034c		; var int 		Y position of the ball rounded to 1 pixel
	
	ball_angle	= $034e		; var char		Ball movement angle (times 15 degrees)
	
	flag_goal	= $034f		; var char 		Set when player scores goal 1=PLAYER1, 2=PLAYER2
	
	score1		= $0350		; var char		Player 1 score
	score2		= $0351		; var char		Player 2 score

	SCORE_POS1 	= 160		; const char	score 1 horizontal location on screen
	SCORE_POS2	= 192		; const char	score 2 location on screen
	
	joy1		= $dc01		
	joy2		= $dc00
		
	var1		= $0352		; var char		Cheap variable 1
	var2		= $0353		; var char		Cheap variable 2
	var3		= $0354		; var char		Cheap variable 3
	
	game_status	= $0355		; var char		=0 Ball is bouncing, =128 p1 is serving, =1 p2 is serving

	counter1	= $0356
	counter2	= $0357

	zp1			= $0fb
	zp2			= $0fd
	
	; basic loader "10 sys 2062"
	
	.byte $0c,$08,$0a,$00,$9e,$20,$32,$30,$36,$32,$00,$00,$00
	
	; add 16-bit integers
	; arg1 int
	; arg2 int
	; result to first argument
	
	add		.macro
			pha
			clc
			lda \1
			adc \2
			sta \1
			lda \1+1
			adc \2+1
			sta \1+1
			pla
			.endm
			
	; convert fixed to integer (/128)
	; arg1 unsigned fixed
	; arg2 int
	; result to second argument
	
	toint	.macro
			pha
			txa
			pha
			lda \1
			sta \2
			lda \1+1
			sta \2+1
			ldx #07
		do	clc
			ror \2+1
			ror \2
			dex
			bne do
			pla
			tax
			pla
			.endm

	; convert integer to fixed (x128)
	; arg1 unsigned integer
	; arg2 fixed
	; result to second argument
	
	tofixed	.macro
			pha
			txa
			pha
			lda \1
			sta \2
			lda \1+1
			sta \2+1
			ldx #07
		do	clc
			rol \2
			rol \2+1
			dex
			bne do
			pla
			tax
			pla
			.endm
			
	; multiply 8-bit numbers
	; by White Flame
	; result in A
			
	mult8	.macro
			lda #$00
 			beq enterloop

		doadd
			clc
			adc \1

		loop
			asl \1
		enterloop
			lsr \2
			bcs doadd
			bne loop
			.endm

	; setup screen

			lda #$00
			sta $d020
			sta $d021

			ldx #$00

	clear	lda #$20
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


			.block
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
			.bend

			; clear sprite area

			.block
			 lda #$00
			 ldx #$ff	 ; fill 256 bytes
		loop
			 sta $3ec0,x
			 dex
			 bne loop		
			.bend
	
			; players shape

			.block
			ldx #00
			lda #$ff
	loop	sta $3f40,x
			inx
			inx
			inx
			cpx #63
			bne loop
			.bend

			; ball shape

			.block
			ldx #00
			lda #$ff
	loop	sta $3f80,x
			inx
			inx
			inx
			cpx #24
			bne loop
			.bend
			
			lda #121		; initial player positions
			sta player_pos1
			sta player_pos2

			jsr configure

			; configure interrupt

			lda #%01111111
			sta $dc0d
			and $d011
			sta $d011
			lda #250
			sta $d012
			lda #<gloop
			sta $0314
			lda #>gloop
			sta $0315

			; set initial scores

			lda #$00
			sta score1
			sta score2
			jsr update_score

	set_new_status

			lda #01
			sta game_status

	; off-game loop
	;

	off_game

			; redirect interrupt to
			; the scoretable flashing
			; routine		

			lda #$00
			sta var1

			lda #<score_flash
			sta $0314
			lda #>score_flash
			sta $0315
			
			lda #%00000001
			sta $d01a

			; wait for game start

	ogloop  lda #%00010000
			bit $dc00
	 		bne j2
			jmp start_new_game

		j2	bit $dc01
			bne ogloop
	

	; game loop
	;

	start_new_game

			lda #%00011111
			sta $d015
			
			lda #$00
			sta score1
			sta score2
			jsr update_score

	start_round

			
			; clear goal flag
			
			lda #$00			
			sta flag_goal;

			; disable interrupt and 
			; reconfigure for gameplay

			lda #%00000000
			sta $d01a

			lda #<gloop
			sta $0314
			lda #>gloop
			sta $0315

			; reset players

			lda #120
			sta player_pos1
			sta player_pos2

			sta $d001
			sta $d003

			jsr update_score

			; start interrupt

			lda #%00000001
			sta $d01a

			; serve
			; wait for serving player fire
	
	srv_loop
			.block

			jsr wait1sec

			lda game_status
			bmi serving1
						
	serving2	
			lda #%00010000
			bit $dc00
			bne serving2
			lda #17
			sta ball_angle
			jsr update_ball_dir
			jmp exit_serving

	serving1
			lda #%00010000
			bit $dc01
			bne serving1
			
	exit_serving
			lda #$00
			sta game_status		

			.bend

	; set loop
	; let them play and wait for goals

	eloop	
			.block
			; was there a goal?
			
			lda flag_goal
			beq no
	
			ldx #%00000000
			stx $d01a

			; yes, update score and exit loop
			cmp #$01
			beq p1
			inc score2
			lda #128
			sta game_status
			jmp ex
		p1 	inc score1
			lda #01
			sta game_status
		ex	jsr update_score

			; anyone has 9 points?
			
			lda #09
			cmp score1
			beq game_over
			cmp score2
			beq game_over

			jmp start_round
		no	jmp eloop


	game_over
			jmp set_new_status
			.bend
	
	; play loop
	; get joystick movements
	; set player positions
	; set ball speed vectors

	gloop
			;lda #$01		; debug
			;sta $d020		; debug
			
			jsr sprpos

			jsr player_movement
			jsr ball_movement				

			dec counter1
			bne skip1
			lda #$16
			sta $d404
	skip1
			dec counter2
			bne skip2
			lda #$16
			sta $d40b

	skip2	asl $d019
			jmp $ea81

	player_movement
	
			.block
			lda #%00000001
			bit joy1
			bne p1down
			lda player_pos1
			cmp #$38
			beq p1down
			dec player_pos1
			dec player_pos1
			jmp p2up
	p1down
			lda #%00000010	
			bit joy1
			bne p2up
			lda player_pos1
			cmp #$ca
			beq p2up
			inc player_pos1
			inc player_pos1
	p2up
			lda #%00000001
			bit joy2
			bne p2down
			lda player_pos2
			cmp #$38
			beq p2down
			dec player_pos2
			dec player_pos2
			jmp plend
	
	p2down
			lda #%00000010	
			bit joy2
			bne plend
			lda player_pos2
			cmp #$ca
			beq plend
			inc player_pos2
			inc player_pos2

	plend
			rts
			.bend

	ball_movement
			.block
			lda game_status
			beq go
			bmi p1serve

	p2serve
			lda player_pos2
			ldx #$02
			jsr pos_ball
			rts
	p1serve
			lda player_pos1
			ldx #$00
			jsr pos_ball
			rts

	pos_ball
			adc #16
			sta var1
			lda #$00
			sta var1+1

			#tofixed var1, ball_posy
		
			lda ball_init_x1,x			
			sta ball_posx
			lda ball_init_x1+1,x			
			sta ball_posx+1

			rts
	
		go	
			#add ball_posx, ball_dx
			#add ball_posy, ball_dy
			rts
			.bend

	; sprite positioning
	
	sprpos	
			.block
			lda #24
			sta $d000
			lda player_pos1
			sta $d001
			
			lda #80
			sta $d002
			lda player_pos2
			sta $d003
			
			#toint ball_posx, ball_posrx
			#toint ball_posy, ball_posry
			
			lda ball_posrx
			sta $d004
			
			lda ball_posry
			sta $d005

			ldx #$02			
			lda ball_posrx+1
			beq	skip
			ldx #$06

	skip	stx $d010
	
			lda game_status
			bne end_sprpos

			; check for border collision
			lda ball_posry		
			clc	
			cmp #$39
			bcc border_collision
			cmp #$ec
			bcs border_collision

			; check for goal

			lda ball_posrx+1
			beq llw
			
			lda ball_posrx
			cmp #80
			lda #$01
			bcs goal
			jmp checkhit

	llw		lda ball_posrx
			cmp #09
			lda #$02
			bcc goal

			; check for player_collision

	checkhit
			lda ball_posrx+1
			beq low
			
			lda ball_posrx
			cmp #72		; player 2 and ball
			lda player_pos2
			bcs player_collision_check
			jmp end_sprpos

	low		lda ball_posrx
			cmp #32		; player 1 and ball
			lda player_pos1
			bcc player_collision_check
			
			;lda ball_posx
			
	
	end_sprpos
			rts

	goal
			sta flag_goal
			jmp end_sprpos
			
	border_collision
			ldx ball_angle
			lda hbounce,x
			sta ball_angle
			jsr klang1
			jsr update_ball_dir
			jmp end_sprpos
	
	player_collision_check	
			adc #21					; middle line of player
			sec
			sbc ball_posry
			bmi negative
	positive
			cmp #28
			bcc player_bounce
			
	negative
			cmp #235
			bcs player_bounce
			jmp end_sprpos			; bellow
			
		player_bounce
			adc #21					; now it's 0-49
			lsr
			lsr
			lsr
			ldy ball_posrx+1
			beq skip2
			adc #$06
		skip2
			tax
			lda vbounce,x
			sta ball_angle
			jsr klang2
			jsr update_ball_dir
			jmp end_sprpos
			.bend
			
	; take the ball angle and
	; update the ball direction x and y vectors
			
	update_ball_dir
			.block
			ldx #$02
			stx var1
			ldy ball_angle
			sty var2
			#mult8 var1, var2
			tax

			lda xvectors,x
			sta ball_dx
			lda xvectors+1,x
			sta ball_dx+1

			lda yvectors,x
			sta ball_dy
			lda yvectors+1,x
			sta ball_dy+1
			rts		
			.bend
	
	configure
	
			.block
			lda #$fb		; sprite shapes
			sta $07fb

			lda #$fc
			sta $07fc

			lda #$fd		
			sta $07f8
			sta $07f9
			lda #$fe
			sta $07fa
			
			lda #%00011111	; 5 sprites on
			sta $d015
			
			lda #%00011011	; players and digits double height
			sta $d017
	
			lda #%00011000	; digits double width
			sta $d01d	
			
			lda #$00		; monochrome sprites
			sta $d01c
			
			lda #$0a		; colors
			sta $d027
			lda #$0e
			sta $d028
			lda #07
			sta $d029
			lda #12
			sta $d02a
			sta $d02b

			lda #$00		; sound
			sta $d400
			sta $d407
			lda #180
			sta $d401
			sta $d408
			lda #$00
			sta $d404
			sta $d40b
			lda #9
			sta $d405
			sta $d40c
			lda #0
			sta $d406
			sta $d40d
			lda #%00000111
			sta $d418
			
			
			lda #SCORE_POS1	; score table position
			sta $d006
			lda #SCORE_POS2
			sta $d008
			lda #60
			sta $d007
			sta $d009
						
			lda #$00
			sta $d01b
			
			lda #$05
			sta ball_angle

			; update ball direction
			jsr update_ball_dir
			jsr sprpos
			
			rts			
			.bend

	; refresh score table

	update_score
			ldx #$00
			lda score1
			jsr draw_digit
			ldx #$01
			lda score2
			jsr draw_digit
			rts

	; draw a digit on screen
	; X which player
	; A digit

	draw_digit
			.block

			sta var1

			lda #$c0
			sta $fb

			lda #$3e
			sta $fc

			cpx #$00
			beq skip			
			
			lda #$00
			sta $fb

			lda #$3f
			sta $fc


	skip	
			lda #10
			sta var2
			#mult8 var1,var2
			tax
	
			ldy #$00
	loop	lda digits,x
			sta ($fb),y
			iny
			lda #$00
			sta ($fb),y
			iny
			sta ($fb),y
			iny
			inx
			cpy #28
			bcc loop

			rts
			.bend

	score_flash
			.block
			inc var1
			lda var1
			cmp #25
			bcc skip
			
			lda $d015
			eor #%00011000
			sta $d015

			lda #$00
			sta var1			

	skip	asl $d019
			jmp $ea81
			.bend

	wait1sec
			.block
			ldx #50
	loop
			lda $d012
			bne loop	
			dex
			bne loop
			rts
			.bend

	klang1
			lda #%00010001
			sta $d404
			lda #35
			sta counter1
			rts

	klang2
			lda #%00010001
			sta $d40b
			lda #35
			sta counter2
			rts
	

	; Sprites
	; ------------------

	digits		
					.byte %11111111
					.byte %11111111
					.byte %11000011
					.byte %11000011
					.byte %11000011
					.byte %11000011
					.byte %11000011
					.byte %11000011
					.byte %11111111
					.byte %11111111

					.repeat 10, %00000011

					.byte %11111111
					.byte %11111111
					.byte %00000011
					.byte %00000011
					.byte %11111111
					.byte %11111111
					.byte %11000000
					.byte %11000000
					.byte %11111111
					.byte %11111111

					.byte %11111111
					.byte %11111111
					.byte %00000011
					.byte %00000011
					.byte %11111111
					.byte %11111111
					.byte %00000011
					.byte %00000011
					.byte %11111111
					.byte %11111111

					.byte %11000011
					.byte %11000011
					.byte %11000011
					.byte %11000011
					.byte %11111111
					.byte %11111111
					.byte %00000011
					.byte %00000011
					.byte %00000011
					.byte %00000011

					.byte %11111111
					.byte %11111111
					.byte %11000000
					.byte %11000000
					.byte %11111111
					.byte %11111111
					.byte %00000011
					.byte %00000011
					.byte %11111111
					.byte %11111111

					.byte %11111111
					.byte %11111111
					.byte %11000000
					.byte %11000000
					.byte %11111111
					.byte %11111111
					.byte %11000011
					.byte %11000011
					.byte %11111111
					.byte %11111111

					.byte %11111111
					.byte %11111111
					.byte %00000011
					.byte %00000011
					.byte %00000011
					.byte %00000011
					.byte %00000011
					.byte %00000011
					.byte %00000011
					.byte %00000011

					.byte %11111111
					.byte %11111111
					.byte %11000011
					.byte %11000011
					.byte %11111111
					.byte %11111111
					.byte %11000011
					.byte %11000011
					.byte %11111111
					.byte %11111111

					.byte %11111111
					.byte %11111111
					.byte %11000011
					.byte %11000011
					.byte %11111111
					.byte %11111111
					.byte %00000011
					.byte %00000011
					.byte %11111111
					.byte %11111111
					
	; Constants
	; ------------------

	ball_init_x1	.byte $00,$14
	ball_init_x2	.byte $00,$a0
	
	vbounce			;.byte 10,9,7,5,3,2,14,15,17,19,21,22
					.byte 3,4,5,5,7,8,9, 22,21,20,16,16,15,14
	
	hbounce			.byte 12,11,10,9,8,7,6,5,4,3,2,1,0,23,22,21,20,19,18,17,16,15,14,13
	
		xvectors  	.word $0000
					.word $0084
					.word $00ff
					.word $016a
					.word $01bb
					.word $01ee

		yvectors  	.word $01ff
					.word $01ee
					.word $01bb
					.word $016a
					.word $0100
					.word $0084
					.word $0000
					.word $ff7b
					.word $ff00
					.word $fe95
					.word $fe44
					.word $fe11
					.word $fe00
					.word $fe11
					.word $fe44
					.word $fe95
					.word $feff
					.word $ff7b

		rvectors  	.word $0000
					.word $0084
					.word $00ff
					.word $016a
					.word $01bb
					.word $01ee
