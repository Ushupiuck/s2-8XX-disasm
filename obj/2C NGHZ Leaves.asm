; ===========================================================================
; ---------------------------------------------------------------------------
; Object 2C - Sprite that makes leaves fly off when you hit it from NGHZ
; ---------------------------------------------------------------------------
; Sprite_1A0C4: Obj_0x2C_Leaves:
Obj2C:
		moveq	#0,d0
		move.b	routine(a0),d0
		move.w	Obj2C_Index(pc,d0.w),d1
		jmp	Obj2C_Index(pc,d1.w)
; ===========================================================================
; off_1A0d2:
Obj2C_Index:	offsetTable
		offsetTableEntry.w Obj2C_Init
		offsetTableEntry.w Obj2C_Main
		offsetTableEntry.w Obj2C_Leaf
; ---------------------------------------------------------------------------
; byte_1A0D8:
Obj2C_CollisionFlags:
		dc.b	$d6
		dc.b	$d4
		dc.b	$d5
		even
; ===========================================================================
; loc_1A0DC:
Obj2C_Init:
		addq.b	#2,routine(a0)
		moveq	#0,d0
		move.b	$28(a0),d0
		move.b	Obj2C_CollisionFlags(pc,d0.w),$20(a0)
		move.l	#Obj31_MapUnc_15612,4(a0)
		move.w	#$8680,2(a0)
		move.b	#$84,1(a0)
		move.b	#$80,$19(a0)
		move.b	#4,$18(a0)
		move.b	$28(a0),$1A(a0)
; loc_1A112:
Obj2C_Main:
		move.w	x_pos(a0),d0
		andi.w	#$FF80,d0
		sub.w	(Camera_X_pos_coarse).w,d0
		cmpi.w	#$280,d0
		bhi.w	JmpTo22_DeleteObject
		; these instructions were deleted from the final,meaning
		; entering edit mode will no longer display its collision box
		tst.w	(Debug_placement_mode).w
		beq.s	loc_1A130
		jsrto	JmpTo7_DisplaySprite

loc_1A130:
		move.b	$21(a0),d0
		beq.s	return_1A16C
		move.b	(Timer_frames+1).w,d0
		andi.w	#$F,d0
		bne.s	loc_1A150
		lea	(MainCharacter).w,a2	; a2=character
		bclr	#0,$21(a0)
		beq.s	Obj2C_RemoveCollision
		bsr.s	Obj2C_CreateLeaves
		bra.s	Obj2C_RemoveCollision

	if RemoveJmpTos
JmpTo22_DeleteObject:
		jmp	(DeleteObject).l
	endif
; ---------------------------------------------------------------------------

loc_1A150:
		addi.w	#8,d0
		andi.w	#$F,d0
		bne.s	Obj2C_RemoveCollision
		lea	(Sidekick).w,a2		; a2=character
		bclr	#1,$21(a0)
		beq.s	Obj2C_RemoveCollision
		bsr.s	Obj2C_CreateLeaves
; loc_1A168:
Obj2C_RemoveCollision:
		clr.b	$21(a0)

return_1A16C:
		rts
; ===========================================================================
; loc_1A16E:
Obj2C_CreateLeaves:
		move.w	$10(a2),d0
		bpl.s	loc_1A176
		neg.w	d0

loc_1A176:
		cmpi.w	#$200,d0
		bhs.s	loc_1A18A
		move.w	$12(a2),d0
		bpl.s	loc_1A184
		neg.w	d0

loc_1A184:
		cmpi.w	#$200,d0
		blo.s	return_1A16C

loc_1A18A:
		lea	(Obj2C_Speeds).l,a3
		moveq	#4-1,d6

loc_1A192:
		jsrto	JmpTo4_SingleObjLoad
		bne.w	loc_1A21E
		_move.b	#$2C,0(a1)		; load obj2C (leaves generator)
		move.b	#4,routine(a1)
		move.w	x_pos(a2),x_pos(a1)
		move.w	y_pos(a2),y_pos(a1)
		jsrto	JmpTo_PseudoRandomNumber
		andi.w	#$F,d0
		subq.w	#8,d0
		add.w	d0,x_pos(a1)
		swap	d0
		andi.w	#$F,d0
		subq.w	#8,d0
		add.w	d0,y_pos(a1)
		move.w	(a3)+,$10(a1)
		move.w	(a3)+,$12(a1)
		btst	#0,$22(a2)
		beq.s	loc_1A1E0
		neg.w	$10(a1)

loc_1A1E0:
		move.w	x_pos(a1),$30(a1)
		move.w	y_pos(a1),$34(a1)
		andi.b	#1,d0
		move.b	d0,$1A(a1)
		move.l	#Obj2C_MapUnc_1A2BC,4(a1)
		move.w	#$E410,2(a1)
		move.b	#$84,1(a1)
		move.b	#8,$19(a1)
		move.b	#1,$18(a1)
		move.b	#4,$38(a1)
	if FixBugs
		move.b	d1,$26(a1)
	else
		; This should be using a1 instead of a0
		move.b	d1,$26(a0)
	endif

loc_1A21E:
		dbf	d6,loc_1A192
		rts
; ===========================================================================
; word_1A224:
Obj2C_Speeds:
		dc.w	-$80,-$80
		dc.w	 $C0,-$40
		dc.w	-$C0,$40
		dc.w	 $80,$80
; ===========================================================================
; loc_1A234:
Obj2C_Leaf:
		move.b	$38(a0),d0
		add.b	d0,$26(a0)
		add.b	(Vint_runcount+3).w,d0
		andi.w	#$1F,d0
		bne.s	loc_1A252
		add.b	d7,d0
		andi.b	#1,d0
		beq.s	loc_1A252
		neg.b	$38(a0)

loc_1A252:
		move.l	$30(a0),d2
		move.l	$34(a0),d3
		move.w	$10(a0),d0
		ext.l	d0
		asl.l	#8,d0
		add.l	d0,d2
		move.w	$12(a0),d0
		ext.l	d0
		asl.l	#8,d0
		add.l	d0,d3
		move.l	d2,$30(a0)
		move.l	d3,$34(a0)
		swap	d2
		andi.w	#3,d3
		addq.w	#4,d3
		add.w	d3,$12(a0)
		move.b	$26(a0),d0
		jsrto	JmpTo3_CalcSine
		asr.w	#6,d0
		add.w	$30(a0),d0
		move.w	d0,x_pos(a0)
		asr.w	#6,d1
		add.w	$34(a0),d1
		move.w	d1,y_pos(a0)
		subq.b	#1,$1E(a0)
		bpl.s	loc_1A2B0
		move.b	#$B,$1E(a0)
		bchg	#1,$1A(a0)

loc_1A2B0:
		tst.b	1(a0)
		bpl.w	JmpTo22_DeleteObject
		jmpto	JmpTo7_DisplaySprite