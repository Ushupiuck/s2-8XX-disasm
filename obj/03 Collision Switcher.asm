; ===========================================================================
; ---------------------------------------------------------------------------
; Object 03 - Collision plane/layer switcher
; ---------------------------------------------------------------------------
; Sprite_144B0: Obj_0x03_Layer_Switch:
Obj03:
		moveq	#0,d0
		move.b	routine(a0),d0
		move.w	Obj03_Index(pc,d0.w),d1
		jsr	Obj03_Index(pc,d1.w)
		tst.w	(Debug_mode_flag).w
		beq.w	MarkObjGone3
		jmp	(MarkObjGone).l
; ===========================================================================
; off_144CC:
Obj03_Index:	offsetTable
		offsetTableEntry.w Obj03_Init
		offsetTableEntry.w Obj03_MainX
		offsetTableEntry.w Obj03_MainY
; ===========================================================================
; loc_144d2:
Obj03_Init:
		addq.b	#2,routine(a0)
		move.l	#Obj03_MapUnc_147d0,4(a0)
		move.w	#$26BC,2(a0)
		bsr.w	Adjust2PArtPointer
		ori.b	#4,1(a0)
		move.b	#$10,$19(a0)
		move.b	#5,$18(a0)
		move.b	$28(a0),d0
		btst	#2,d0
		beq.s	Obj03_Init_CheckX

; Obj03_Init_CheckY:
		addq.b	#2,routine(a0)	; => Obj03_MainY
		andi.w	#7,d0
		move.b	d0,$1A(a0)
		andi.w	#3,d0
		add.w	d0,d0
		move.w	Obj03_Sizes(pc,d0.w),$32(a0)
		bra.w	Obj03_MainY
; ===========================================================================
; word_14520:
Obj03_Sizes:	dc.w	$20,$40,$80,$100
; ===========================================================================
; loc_14528:
Obj03_Init_CheckX:
		andi.w	#3,d0
		move.b	d0,$1A(a0)
		add.w	d0,d0
		move.w	Obj03_Sizes(pc,d0.w),$32(a0)

; loc_14538:
Obj03_MainX:
		tst.w	(Debug_placement_mode).w
		bne.w	return_1465A
		move.b	#0,$34(a0)
		move.w	$30(a0),d5
		move.w	x_pos(a0),d0
		move.w	d0,d1
		subq.w	#8,d0
		addq.w	#8,d1
		move.w	y_pos(a0),d2
		move.w	d2,d3
		move.w	$32(a0),d4
		sub.w	d4,d2
		add.w	d4,d3
		lea	(Obj03_Characters).l,a2
		moveq	#7,d6

loc_1456A:
		move.l	(a2)+,d4
		beq.w	loc_1464A
		move.l	d4,a1
		move.w	x_pos(a1),d4
		cmp.w	d0,d4
		blo.w	loc_1459A
		cmp.w	d1,d4
		bhs.w	loc_1459A
		move.w	y_pos(a1),d4
		cmp.w	d2,d4
		blo.w	loc_1459A
		cmp.w	d3,d4
		bhs.w	loc_1459A
		ori.w	#$8000,d5
		bra.w	loc_1464A
; ===========================================================================

loc_1459A:
		tst.w	d5
		bpl.w	loc_1464A
		swap	d0
		move.b	$28(a0),d0
		bpl.s	loc_145B2
		btst	#1,$22(a1)
		bne.w	loc_14644

loc_145B2:
		move.w	x_pos(a1),d4
		cmp.w	x_pos(a0),d4
		blo.s	loc_145F6
		btst	#0,1(a0)
		bne.s	loc_145E2
		move.b	#$C,$3E(a1)
		move.b	#$D,$3F(a1)
		btst	#3,d0
		beq.s	loc_145E2
		move.b	#$E,$3E(a1)
		move.b	#$F,$3F(a1)

loc_145E2:
		bclr	#7,2(a1)
		btst	#5,d0
		beq.s	loc_1462E
		bset	#7,2(a1)
		bra.s	loc_1462E
; ===========================================================================

loc_145F6:
		btst	#0,1(a0)
		bne.s	loc_1461C
		move.b	#$C,$3E(a1)
		move.b	#$D,$3F(a1)
		btst	#4,d0
		beq.s	loc_1461C
		move.b	#$E,$3E(a1)
		move.b	#$F,$3F(a1)

loc_1461C:
		bclr	#7,2(a1)
		btst	#6,d0
		beq.s	loc_1462E
		bset	#7,2(a1)

loc_1462E:
		move.b	#1,$34(a0)
		tst.w	(Debug_mode_flag).w
		beq.s	loc_14644
		move.w	#SndID_Checkpoint,d0
		jsr	(PlaySound).l

loc_14644:
		swap	d0
		andi.w	#$7FFF,d5

loc_1464A:
		add.l	d5,d5
		dbf	d6,loc_1456A
		swap	d5
		move.b	d5,$30(a0)
		bsr.w	loc_147A0

return_1465A:
		rts
; ===========================================================================
; loc_1465C:
Obj03_MainY:
		tst.w	(Debug_placement_mode).w
		bne.w	return_1477E
		move.b	#0,$34(a0)
		move.w	$30(a0),d5
		move.w	$08(a0),d0
		move.w	d0,d1
		move.w	$32(a0),d4
		sub.w	d4,d0
		add.w	d4,d1
		move.w	y_pos(a0),d2
		move.w	d2,d3
		subq.w	#8,d2
		addq.w	#8,d3
		lea	(Obj03_Characters).l,a2
		moveq	#7,d6

loc_1468E:
		move.l	(a2)+,d4
		beq.w	loc_1476E
		move.l	d4,a1
		move.w	x_pos(a1),d4
		cmp.w	d0,d4
		blo.w	loc_146BE
		cmp.w	d1,d4
		bhs.w	loc_146BE
		move.w	y_pos(a1),d4
		cmp.w	d2,d4
		blo.w	loc_146BE
		cmp.w	d3,d4
		bhs.w	loc_146BE
		ori.w	#$8000,d5
		bra.w	loc_1476E
; ===========================================================================

loc_146BE:
		tst.w	d5
		bpl.w	loc_1476E
		swap	d0
		move.b	$28(a0),d0
		bpl.s	loc_146d6
		btst	#1,$22(a1)
		bne.w	loc_14768

loc_146d6:
		move.w	y_pos(a1),d4
		cmp.w	y_pos(a0),d4
		blo.s	loc_1471A
		btst	#0,1(a0)
		bne.s	loc_14706
		move.b	#$C,$3E(a1)
		move.b	#$D,$3F(a1)
		btst	#3,d0
		beq.s	loc_14706
		move.b	#$E,$3E(a1)
		move.b	#$F,$3F(a1)

loc_14706:
		bclr	#7,2(a1)
		btst	#5,d0
		beq.s	loc_14752
		bset	#7,2(a1)
		bra.s	loc_14752
; ===========================================================================

loc_1471A:
		btst	#0,1(a0)
		bne.s	loc_14740
		move.b	#$C,$3E(a1)
		move.b	#$D,$3F(a1)
		btst	#4,d0
		beq.s	loc_14740
		move.b	#$E,$3E(a1)
		move.b	#$F,$3F(a1)

loc_14740:
		bclr	#7,2(a1)
		btst	#6,d0
		beq.s	loc_14752
		bset	#7,2(a1)

loc_14752:
		move.b	#1,$34(a0)
		tst.w	(Debug_mode_flag).w
		beq.s	loc_14768
		move.w	#SndID_Checkpoint,d0
		jsr	(PlaySound).l

loc_14768:
		swap	d0
		andi.w	#$7FFF,d5

loc_1476E:
		add.l	d5,d5
		dbf	d6,loc_1468E
		swap	d5
		move.b	d5,$30(a0)
		bsr.w	loc_147A0

return_1477E:
		rts
; ===========================================================================
; dword_14780:
Obj03_Characters:
		; character 1,character 2
		dc.l	MainCharacter,Sidekick
		dc.l	0,0
		dc.l	0,0
		dc.l	0,0
		even
; ===========================================================================

loc_147A0:
		tst.b	$34(a0)
		beq.s	return_147CE
		tst.w	(MainCharacter+2).w
		bpl.s	loc_147B4
		bset	#7,(Shield+2).w
		bra.s	loc_147BA

loc_147B4:
		bclr	#7,(Shield+2).w

loc_147BA:
		tst.w	(Sidekick+2).w
		bpl.s	loc_147C8
		bset	#7,(Tails_Tails+2).w
		bra.s	return_147CE

loc_147C8:
		bclr	#7,(Tails_Tails+2).w

return_147CE:
		rts