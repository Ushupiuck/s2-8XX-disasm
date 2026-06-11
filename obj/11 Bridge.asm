; ===========================================================================
; ---------------------------------------------------------------------------
; Object 11 - Bridge in Green Hill Zone and Hidden Palace Zone
; ---------------------------------------------------------------------------
; Sprite_7FDC: Obj_0x11_Bridge:
Obj11:
		btst	#6,render_flags(a0)	; is this a child sprite object?
		bne.w	Obj11_DrawChild		; if yes,branch
		moveq	#0,d0
		move.b	routine(a0),d0
		move.w	Obj11_Index(pc,d0.w),d1
		jmp	Obj11_Index(pc,d1.w)
; ===========================================================================
; loc_7FF4:
Obj11_DrawChild:
		move.w	#$180,d0
		bra.w	DisplaySprite3
; ===========================================================================
; off_7FFC:
Obj11_Index:	offsetTable
		offsetTableEntry.w Obj11_Init
		offsetTableEntry.w Obj11_GHZ
		offsetTableEntry.w Obj11_Display
		offsetTableEntry.w Obj11_HPZ
; ===========================================================================
; loc_8004:
Obj11_Init:
		addq.b	#2,routine(a0)
		move.l	#Obj11_MapUnc_85E0,mappings(a0)
		move.w	#$43C6,art_tile(a0)
		move.b	#3,priority(a0)
		cmpi.b	#hidden_palace_zone,(Current_Zone).w	; are we in HPZ?
		bne.s	+			; if not,branch
		addq.b	#4,routine(a0)
		move.l	#Obj11_MapUnc_8598,mappings(a0)
		move.w	#$6300,art_tile(a0)
+
		bsr.w	Adjust2PArtPointer
		move.b	#4,render_flags(a0)
		move.b	#$80,width_pixels(a0)
		move.w	y_pos(a0),d2
		move.w	d2,$3C(a0)
		move.w	x_pos(a0),d3
		lea	$28(a0),a2		; copy bridge subtype to a2
		moveq	#0,d1
		move.b	(a2),d1			; d1 = subtype
		move.w	d1,d0
		lsr.w	#1,d0
		lsl.w	#4,d0			; (d0 div 2) * 16
		sub.w	d0,d3			; x position of left half
		swap	d1			; store subtype in high word for later
		move.w	#8,d1
		bsr.s	Obj11_MakeBdgSegment
		move.w	$28(a1),d0
		subq.w	#8,d0
		move.w	d0,x_pos(a1)		; center of first subsprite object
		move.l	a1,$30(a0)		; pointer to first subsprite object
		swap	d1
		subq.w	#8,d1
		bls.s	+			; branch,if subtype <= 8 (bridge has no more than 8 logs)
		; else,create a second subsprite object for the rest of the bridge
		move.w	d1,d4
		bsr.s	Obj11_MakeBdgSegment
		move.l	a1,$34(a0)		; pointer to second subsprite object
		move.w	d4,d0
		add.w	d0,d0
		add.w	d4,d0
		move.w	$10(a1,d0.w),d0
		subq.w	#8,d0
		move.w	d0,x_pos(a1)		; center of second subsprite object
+
		bra.s	Obj11_GHZ

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; sub_8098:
Obj11_MakeBdgSegment:
		bsr.w	SingleObjLoad2
		bne.s	+
		_move.b	id(a0),id(a1)		; load obj11
		move.w	x_pos(a0),x_pos(a1)
		move.w	y_pos(a0),y_pos(a1)
		move.l	mappings(a0),mappings(a1)
		move.w	art_tile(a0),art_tile(a1)
		move.b	render_flags(a0),render_flags(a1)
		bset	#6,render_flags(a1)
		move.b	#$40,mainspr_width(a1)
		move.b	d1,mainspr_childsprites(a1)
		subq.b	#1,d1
		lea	subspr_data(a1),a2	; starting address for subsprite data

-		move.w	d3,(a2)+		; sub?_x_pos
		move.w	d2,(a2)+		; sub?_y_pos
		move.w	#0,(a2)+		; sub?_mapframe
		addi.w	#$10,d3			; width of a log,x_pos for next log
		dbf	d1,-			; repeat for d1 logs
+
		rts
; End of function Obj11_MakeBdgSegment

; ===========================================================================
; loc_80EA:
Obj11_GHZ:
		move.b	$22(a0),d0
		andi.b	#$18,d0
		bne.s	loc_8100
		tst.b	$3E(a0)
		beq.s	loc_812C
		subq.b	#4,$3E(a0)
		bra.s	loc_8128

loc_8100:
		andi.b	#$10,d0
		beq.s	loc_811C
		move.b	$3F(a0),d0
		sub.b	$3B(a0),d0
		beq.s	loc_811C
		bhs.s	loc_8118
		addq.b	#1,$3F(a0)
		bra.s	loc_811C
; ---------------------------------------------------------------------------

loc_8118:
		subq.b	#1,$3F(a0)

loc_811C:
		cmpi.b	#$40,$3E(a0)
		beq.s	loc_8128
		addq.b	#4,$3E(a0)

loc_8128:
		bsr.w	Obj11_Depress

loc_812C:
		moveq	#0,d1
		move.b	$28(a0),d1
		lsl.w	#3,d1
		move.w	d1,d2
		addq.w	#8,d1
		add.w	d2,d2
		moveq	#8,d3
		move.w	x_pos(a0),d4
		bsr.w	Obj11_Solid
; loc_8144:
Obj11_Unload:
		; this is essentially MarkObjGone,except we need to delete our subsprite objects as well
		tst.w	(Two_player_mode).w
		beq.s	+
		rts
; ---------------------------------------------------------------------------
+
		move.w	x_pos(a0),d0
		andi.w	#$FF80,d0
		sub.w	(Camera_X_pos_coarse).w,d0
		cmpi.w	#$280,d0
		bhi.s	+
		rts
; ---------------------------------------------------------------------------
+		; delete first subsprite object
		movea.l	$30(a0),a1	; a1=object
		bsr.w	DeleteObject2
		cmpi.b	#8,$28(a0)
		bls.s	+		; if bridge has more than 8 logs,delete second subsprite object
		movea.l	$34(a0),a1	; a1=object
		bsr.w	DeleteObject2
+
		bra.w	DeleteObject
; ===========================================================================
; loc_817C:
Obj11_Display:
		bra.w	DisplaySprite
; ===========================================================================
; loc_8180:
Obj11_HPZ:
		move.b	$22(a0),d0
		andi.b	#$18,d0
		bne.s	loc_8196
		tst.b	$3E(a0)
		beq.s	loc_81C2
		subq.b	#4,$3E(a0)
		bra.s	loc_81BE
; ===========================================================================

loc_8196:
		andi.b	#$10,d0
		beq.s	loc_81B2
		move.b	$3F(a0),d0
		sub.b	$3B(a0),d0
		beq.s	loc_81B2
		bhs.s	loc_81AE
		addq.b	#1,$3F(a0)
		bra.s	loc_81B2

loc_81AE:
		subq.b	#1,$3F(a0)
; ===========================================================================

loc_81B2:
		cmpi.b	#$40,$3E(a0)
		beq.s	loc_81BE
		addq.b	#4,$3E(a0)

loc_81BE:
		bsr.w	Obj11_Depress

loc_81C2:
		moveq	#0,d1
		move.b	$28(a0),d1
		lsl.w	#3,d1
		move.w	d1,d2
		addq.w	#8,d1
		add.w	d2,d2
		moveq	#8,d3
		move.w	x_pos(a0),d4
		bsr.w	Obj11_Solid
		bsr.w	sub_8282
		bra.w	Obj11_Unload

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_81E2:
Obj11_Solid:
		lea	(Sidekick).w,a1		; a1=character
		moveq	#4,d6
		moveq	#$3B,d5
		movem.l	d1-d4,-(sp)
		bsr.s	loc_81FC
		movem.l	(sp)+,d1-d4
		lea	(MainCharacter).w,a1	; a1=character
		subq.b	#1,d6
		moveq	#$3F,d5

loc_81FC:
		btst	d6,$22(a0)
		beq.s	loc_8260
		btst	#1,$22(a1)
		bne.s	loc_821C
		moveq	#0,d0
		move.w	x_pos(a1),d0
		sub.w	x_pos(a0),d0
		add.w	d1,d0
		bmi.s	loc_821C
		cmp.w	d2,d0
		blo.s	loc_822A

loc_821C:
		bclr	#3,$22(a1)
		bclr	d6,$22(a0)
		moveq	#0,d4
		rts
; ===========================================================================

loc_822A:
		lsr.w	#4,d0
		move.b	d0,(a0,d5.w)
		movea.l	$30(a0),a2
		cmpi.w	#8,d0
		blo.s	loc_8242
		movea.l	$34(a0),a2	; a2=character
		subi.w	#8,d0

loc_8242:
		add.w	d0,d0
		move.w	d0,d1
		add.w	d0,d0
		add.w	d1,d0
		move.w	$12(a2,d0.w),d0
		subq.w	#8,d0
		moveq	#0,d1
		move.b	$16(a1),d1
		sub.w	d1,d0
		move.w	d0,y_pos(a1)
		moveq	#0,d4
		rts
; ===========================================================================

loc_8260:
		move.w	d1,-(sp)
		bsr.w	loc_FA7A
		move.w	(sp)+,d1
		btst	d6,$22(a0)
		beq.s	+
		moveq	#0,d0
		move.w	x_pos(a1),d0
		sub.w	x_pos(a0),d0
		add.w	d1,d0
		lsr.w	#4,d0
		move.b	d0,(a0,d5.w)
+
		rts
; End of function sub_81E2


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_8282:
		moveq	#0,d0
		tst.w	(MainCharacter+$10).w
		bne.s	loc_8294
		move.b	(Vint_runcount+3).w,d0
		andi.w	#$1C,d0
		lsr.w	#1,d0

loc_8294:
		moveq	#0,d2
		move.b	byte_82C0+1(pc,d0.w),d2
		swap	d2
		move.b	byte_82C0(pc,d0.w),d2
		moveq	#0,d0
		tst.w	(Sidekick+$10).w
		bne.s	loc_82B2
		move.b	(Vint_runcount+3).w,d0
		andi.w	#$1C,d0
		lsr.w	#1,d0

loc_82B2:
		moveq	#0,d6
		move.b	byte_82C0+1(pc,d0.w),d6
		swap	d6
		move.b	byte_82C0(pc,d0.w),d6
		bra.s	loc_82d0
; ===========================================================================
byte_82C0:
		dc.b   1, 2
		dc.b   1, 2	; 2
		dc.b   1, 2	; 4
		dc.b   1, 2	; 6
		dc.b   0, 1	; 8
		dc.b   0, 0	; 10
		dc.b   0, 0	; 12
		dc.b   0, 1	; 14
; ===========================================================================

loc_82d0:
		moveq	#-2,d3
		moveq	#-2,d4
		move.b	$22(a0),d0
		andi.b	#8,d0
		beq.s	loc_82E2
		move.b	$3F(a0),d3

loc_82E2:
		move.b	$22(a0),d0
		andi.b	#$10,d0
		beq.s	loc_82F0
		move.b	$3B(a0),d4

loc_82F0:
		move.l	$30(a0),a1
		lea	$45(a1),a2
		lea	$15(a1),a1
		moveq	#0,d1
		move.b	$28(a0),d1
		subq.b	#1,d1
		moveq	#0,d5

loc_8306:
		moveq	#0,d0
		subq.w	#1,d3
		cmp.b	d3,d5
		bne.s	loc_8310
		move.w	d2,d0

loc_8310:
		addq.w	#2,d3
		cmp.b	d3,d5
		bne.s	loc_8318
		move.w	d2,d0

loc_8318:
		subq.w	#1,d3
		subq.w	#1,d4
		cmp.b	d4,d5
		bne.s	loc_8322
		move.w	d6,d0

loc_8322:
		addq.w	#2,d4
		cmp.b	d4,d5
		bne.s	loc_832A
		move.w	d6,d0

loc_832A:
		subq.w	#1,d4
		cmp.b	d3,d5
		bne.s	loc_8336
		swap	d2
		move.w	d2,d0
		swap	d2

loc_8336:
		cmp.b	d4,d5
		bne.s	loc_8340
		swap	d6
		move.w	d6,d0
		swap	d6

loc_8340:
		move.b	d0,(a1)
		addq.w	#1,d5
		addq.w	#6,a1
		cmpa.w	a2,a1
		bne.s	loc_8352
		move.l	$34(a0),a1
		lea	$15(a1),a1

loc_8352:
		dbf	d1,loc_8306
		rts
; End of function sub_8282

; ===========================================================================
; subroutine to make the bridge push down where Sonic or Tails walks over
; loc_8358:
Obj11_Depress:
		move.b	$3E(a0),d0
		bsr.w	CalcSine
		move.w	d0,d4
		lea	(byte_8498).l,a4
		moveq	#0,d0
		move.b	$28(a0),d0
		lsl.w	#4,d0
		moveq	#0,d3
		move.b	$3F(a0),d3
		move.w	d3,d2
		add.w	d0,d3
		moveq	#0,d5
		; this "-$80" is here since $80 bytes of data for bridges with 1-7
		; logs were removed,as Sonic 2 only uses bridges with 8-16 logs
		lea	(Obj11_DepressionOffsets-$80).l,a5
		move.b	(a5,d3.w),d5
		andi.w	#$F,d3
		lsl.w	#4,d3
		lea	(a4,d3.w),a3
		movea.l	$30(a0),a1
		lea	$42(a1),a2
		lea	$12(a1),a1

-		moveq	#0,d0
		move.b	(a3)+,d0
		addq.w	#1,d0
		mulu.w	d5,d0
		mulu.w	d4,d0
		swap	d0
		add.w	$3C(a0),d0
		move.w	d0,(a1)
		addq.w	#6,a1
		cmpa.w	a2,a1
		bne.s	+
		movea.l	$34(a0),a1	; a1=object
		lea	$12(a1),a1
+		dbf	d2,-

		moveq	#0,d0
		move.b	$28(a0),d0
		moveq	#0,d3
		move.b	$3F(a0),d3
		addq.b	#1,d3
		sub.b	d0,d3
		neg.b	d3
		bmi.s	++
		move.w	d3,d2
		lsl.w	#4,d3
		lea	(a4,d3.w),a3
		adda.w	d2,a3
		subq.w	#1,d2
		blo.s	++

-		moveq	#0,d0
		move.b	-(a3),d0
		addq.w	#1,d0
		mulu.w	d5,d0
		mulu.w	d4,d0
		swap	d0
		add.w	$3C(a0),d0
		move.w	d0,(a1)
		addq.w	#6,a1
		cmpa.w	a2,a1
		bne.s	+
		movea.l	$34(a0),a1	; a1=object
		lea	$12(a1),a1
+		dbf	d2,-
+
		rts
; ===========================================================================
; seems to be bridge piece vertical position offset data
; byte_8408:
Obj11_DepressionOffsets:
		dc.b   2, 4, 6, 8, 8, 6, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0; 8 logs
		dc.b   2, 4, 6, 8,$A, 8, 6, 4, 2, 0, 0, 0, 0, 0, 0, 0; 9 logs
		dc.b   2, 4, 6, 8,$A,$A, 8, 6, 4, 2, 0, 0, 0, 0, 0, 0; 10 logs
		dc.b   2, 4, 6, 8,$A,$C,$A, 8, 6, 4, 2, 0, 0, 0, 0, 0; 11 logs
		dc.b   2, 4, 6, 8,$A,$C,$C,$A, 8, 6, 4, 2, 0, 0, 0, 0; 12 logs
		dc.b   2, 4, 6, 8,$A,$C,$E,$C,$A, 8, 6, 4, 2, 0, 0, 0; 13 logs
		dc.b   2, 4, 6, 8,$A,$C,$E,$E,$C,$A, 8, 6, 4, 2, 0, 0; 14 logs
		dc.b   2, 4, 6, 8,$A,$C,$E,$10,$E,$C,$A, 8, 6, 4, 2, 0; 15 logs
		dc.b   2, 4, 6, 8,$A,$C,$E,$10,$10,$E,$C,$A, 8, 6, 4, 2; 16 logs
		even

; something else important for bridge depression to work (phase? bridge size adjustment?)
byte_8498:
		dc.b $FF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0; 16
		dc.b $B5,$FF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0; 32
		dc.b $7E,$DB,$FF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0; 48
		dc.b $61,$B5,$EC,$FF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0; 64
		dc.b $4A,$93,$CD,$F3,$FF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0; 80
		dc.b $3E,$7E,$B0,$DB,$F6,$FF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0; 96
		dc.b $38,$6D,$9D,$C5,$E4,$F8,$FF, 0, 0, 0, 0, 0, 0, 0, 0, 0; 112
		dc.b $31,$61,$8E,$B5,$d4,$EC,$FB,$FF, 0, 0, 0, 0, 0, 0, 0, 0; 128
		dc.b $2B,$56,$7E,$A2,$C1,$DB,$EE,$FB,$FF, 0, 0, 0, 0, 0, 0, 0; 144
		dc.b $25,$4A,$73,$93,$B0,$CD,$E1,$F3,$FC,$FF, 0, 0, 0, 0, 0, 0; 160
		dc.b $1F,$44,$67,$88,$A7,$BD,$d4,$E7,$F4,$FD,$FF, 0, 0, 0, 0, 0; 176
		dc.b $1F,$3E,$5C,$7E,$98,$B0,$C9,$DB,$EA,$F6,$FD,$FF, 0, 0, 0, 0; 192
		dc.b $19,$38,$56,$73,$8E,$A7,$BD,$d1,$E1,$EE,$F8,$FE,$FF, 0, 0, 0; 208
		dc.b $19,$38,$50,$6D,$83,$9D,$B0,$C5,$D8,$E4,$F1,$F8,$FE,$FF, 0, 0; 224
		dc.b $19,$31,$4A,$67,$7E,$93,$A7,$BD,$CD,$DB,$E7,$F3,$F9,$FE,$FF, 0; 240
		dc.b $19,$31,$4A,$61,$78,$8E,$A2,$B5,$C5,$d4,$E1,$EC,$F4,$FB,$FE,$FF; 256
		even