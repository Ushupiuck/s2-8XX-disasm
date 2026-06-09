;===============================================================================
; Object 0x50 - Oil Ocean - Aquis (Seahorse)
; [ Begin ]
;===============================================================================
Obj_0x50_Aquis: ; loc_1E010:
		moveq	#0,d0
		move.b	routine(a0),d0
		move.w	loc_1E01E(pc,d0.w),d1
		jmp	loc_1E01E(pc,d1.w)
loc_1E01E:
		dc.w	loc_1E02A-loc_1E01E
		dc.w	loc_1E0E2-loc_1E01E
		dc.w	loc_1E10E-loc_1E01E
		dc.w	loc_1E138-loc_1E01E
		dc.w	loc_1E31A-loc_1E01E
		dc.w	loc_1E356-loc_1E01E
loc_1E02A:
		addq.b	#2,routine(a0)
		move.l	#Aquis_Mappings,mappings(a0) ; loc_1E4E0
		move.w	#$2570,art_tile(a0)
		ori.b	#4,$0001(a0)
		move.b	#$A,$0020(a0)
		move.b	#4,$0018(a0)
		move.b	#$10,$0019(a0)
		move.w	#$FF00,$0010(a0)
		move.b	$0028(a0),d0
		move.b	d0,d1
		andi.w	#$F0,d1
		lsl.w	#4,d1
		move.w	d1,$002E(a0)
		move.w	d1,$0030(a0)
		andi.w	#$F,d0
		lsl.w	#4,d0
		subq.w	#1,d0
		move.w	d0,$0032(a0)
		move.w	d0,$0034(a0)
		move.w	y_pos(a0),$002A(a0)
		jsrto	JmpTo8_SingleObjLoad
		bne.s	loc_1E0E2
		_move.b	#id_Obj50,id(a1)
		move.b	#4,$0024(a1)
		move.w	x_pos(a0),x_pos(a1)
		move.w	y_pos(a0),y_pos(a1)
		addi.w	#$A,x_pos(a1)
		addi.w	#$FFFA,y_pos(a1)
		move.l	#Aquis_Mappings,mappings(a1) ; loc_1E4E0
		move.w	#$24E0,art_tile(a1)
		ori.b	#4,$0001(a1)
		move.b	#3,$0018(a1)
		move.b	$0022(a0),$0022(a1)
		move.b	#3,$001C(a1)
		move.l	A1,$0036(a0)
		move.l	A0,$0036(a1)
		bset	#6,$0022(a0)
loc_1E0E2:
		lea	(loc_1E4A8).l,A1
		jsrto	JmpTo9_AnimateSprite
		move.w	#$39C,(Water_Level_1).w
		moveq	#0,d0
		move.b	$0025(a0),d0
		move.w	loc_1E108(pc,d0.w),d1
		jsr	loc_1E108(pc,d1.w)
		bsr.w	loc_1E2E0
		jmpto	JmpTo22_MarkObjGone
loc_1E108:
		dc.w	loc_1E14E-loc_1E108
		dc.w	loc_1E160-loc_1E108
		dc.w	loc_1E16E-loc_1E108
loc_1E10E:
		movea.l	$0036(a0),A1
		tst.b	id(a1)
		beq.w	JmpTo36_DeleteObject
		cmpi.b	#id_Obj50,id(a1)
		bne.w	JmpTo36_DeleteObject
		btst	#7,$0022(a1)
		bne.w	JmpTo36_DeleteObject
		lea	(loc_1E4A8).l,A1
		jsrto	JmpTo9_AnimateSprite
		jmpto	JmpTo16_DisplaySprite
loc_1E138:
		bsr.w	loc_1E404
		jsrto	JmpTo14_ObjectMove
		lea	(loc_1E4A8).l,A1
		jsrto	JmpTo9_AnimateSprite
		jmpto	JmpTo22_MarkObjGone
loc_1E14E:
		jsrto	JmpTo14_ObjectMove
		bsr.w	loc_1E3E6
		bsr.w	loc_1E28C
		bsr.w	loc_1E224
		rts
loc_1E160:
		jsrto	JmpTo14_ObjectMove
		bsr.w	loc_1E3E6
		bsr.w	loc_1E2AE
		rts
loc_1E16E:
		jsrto	JmpTo4_ObjectMoveAndFall
		bsr.w	loc_1E3E6
		bsr.w	loc_1E180
		bsr.w	loc_1E1FC
		rts
loc_1E180:
		tst.b	$002D(a0)
		bne.s	loc_1E18C
		tst.w	$0012(a0)
		bpl.s	loc_1E18E
loc_1E18C:
		rts
loc_1E18E:
		st		$002D(a0)
		jsrto	JmpTo8_SingleObjLoad
		bne.s	loc_1E1FA
		_move.b	#id_Obj50,id(a1)
		move.b	#6,$0024(a1)
		move.w	x_pos(a0),x_pos(a1)
		move.w	y_pos(a0),y_pos(a1)
		move.l	#Aquis_Mappings,mappings(a1) ; loc_1E4E0
		move.w	#$24E0,art_tile(a1)
		ori.b	#4,$0001(a1)
		move.b	#3,$0018(a1)
		move.b	#$E5,$0020(a1)
		move.b	#2,$001C(a1)
		move.w	#$C,d0
		move.w	#$10,d1
		move.w	#-$300,d2
		btst	#0,$0022(a0)
		beq.s	loc_1E1EE
		neg.w	d1
		neg.w	d2
loc_1E1EE:
		sub.w	d0,y_pos(a1)
		sub.w	d1,x_pos(a1)
		move.w	d2,$0010(a1)
loc_1E1FA:
		rts
loc_1E1FC:
		move.w	y_pos(a0),d0
		cmp.w	(Water_Level_1).w,d0
		blt.s	loc_1E222
		move.b	#2,$0025(a0)
		move.b	#0,$001C(a0)
		move.w	$0030(a0),$002E(a0)
		move.w	#$40,$0012(a0)
		sf		$002D(a0)
loc_1E222:
		rts
loc_1E224:
		tst.b	$002C(a0)
		beq.s	loc_1E28A
		move.w	(MainCharacter+8).w,d0
		move.w	(MainCharacter+$C).w,d1
		sub.w	y_pos(a0),d1
		bpl.s	loc_1E28A
		cmpi.w	#$FFD0,d1
		blt.s	loc_1E28A
		sub.w	x_pos(a0),d0
		cmpi.w	#$48,d0
		bgt.s	loc_1E28A
		cmpi.w	#$FFB8,d0
		blt.s	loc_1E28A
		tst.w	d0
		bpl.s	loc_1E262
		cmpi.w	#$FFD8,d0
		bgt.s	loc_1E28A
		btst	#0,$0022(a0)
		bne.s	loc_1E28A
		bra.s	loc_1E270
loc_1E262:
		cmpi.w	#$28,d0
		blt.s	loc_1E28A
		btst	#0,$0022(a0)
		beq.s	loc_1E28A
loc_1E270:
		moveq	#$20,d0
		cmp.w	$0032(a0),d0
		bgt.s	loc_1E28A
		move.b	#4,$0025(a0)
		move.b	#1,$001C(a0)
		move.w	#$FC00,$0012(a0)
loc_1E28A:
		rts
loc_1E28C:
		subq.w	#1,$002E(a0)
		bne.s	loc_1E2AC
		move.w	$0030(a0),$002E(a0)
		addq.b	#2,$0025(a0)
		move.w	#$FFC0,d0
		tst.b	$002C(a0)
		beq.s	loc_1E2A8
		neg.w	d0
loc_1E2A8:
		move.w	d0,$0012(a0)
loc_1E2AC:
		rts
loc_1E2AE:
		move.w	y_pos(a0),d0
		tst.b	$002C(a0)
		bne.s	loc_1E2CC
		cmp.w	(Water_Level_1).w,d0
		bgt.s	loc_1E2CA
		subq.b	#2,$0025(a0)
		st		$002C(a0)
		clr.w	$0012(a0)
loc_1E2CA:
		rts
loc_1E2CC:
		cmp.w	$002A(a0),d0
		blt.s	loc_1E2CA
		subq.b	#2,$0025(a0)
		sf		$002C(a0)
		clr.w	$0012(a0)
		rts
loc_1E2E0:
		moveq	#$A,d0
		moveq	#-6,d1
		move.l	$0036(a0),A1
		move.w	x_pos(a0),x_pos(a1)
		move.w	y_pos(a0),y_pos(a1)
		move.b	$0022(a0),$0022(a1)
		move.b	$0023(a0),$0023(a1)
		move.b	$0001(a0),$0001(a1)
		btst	#0,$0022(a1)
		beq.s	loc_1E310
		neg.w	d0
loc_1E310:
		add.w	d0,x_pos(a1)
		add.w	d1,y_pos(a1)
		rts
loc_1E31A:
		jsrto	JmpTo4_ObjectMoveAndFall
		bsr.w	loc_1E330
		lea	(loc_1E4A8).l,A1
		jsrto	JmpTo9_AnimateSprite
		jmpto	JmpTo22_MarkObjGone
loc_1E330:
		jsr	(ObjHitFloor).l			 ; (loc_13898)
		tst.w	d1
		bpl.s	loc_1E34A
		add.w	d1,y_pos(a0)
		move.w	$0012(a0),d0
		asr.w	#1,d0
		neg.w	d0
		move.w	d0,$0012(a0)
loc_1E34A:
		subi.b	#1,$0021(a0)
		beq.w	JmpTo36_DeleteObject
		rts
loc_1E356:
		bsr.w	loc_1E3A6
		tst.b	$0025(a0)
		beq.s	loc_1E396
		subi.w	#1,$002C(a0)
		beq.w	JmpTo36_DeleteObject
		move.w	(MainCharacter+8).w,x_pos(a0)
		move.w	(MainCharacter+$C).w,y_pos(a0)
		addi.w	#$C,y_pos(a0)
		subi.b	#1,$002A(a0)
		bne.s	loc_1E398
		move.b	#3,$002A(a0)
		bchg	#0,$0022(a0)
		bchg	#0,$0001(a0)
loc_1E396:
		rts
loc_1E398:
		lea	(loc_1E4A8).l,A1
		jsrto	JmpTo9_AnimateSprite
		jmpto	JmpTo16_DisplaySprite
loc_1E3A6:
		tst.b	$0025(a0)
		bne.s	loc_1E3E4
		move.b	(MainCharacter+routine).w,d0
		cmpi.b	#2,d0
		bne.s	loc_1E3E4
		move.w	(MainCharacter+8).w,x_pos(a0)
		move.w	(MainCharacter+$C).w,y_pos(a0)
		ori.b	#4,$0001(a0)
		move.b	#1,$0018(a0)
		move.b	#5,$001C(a0)
		st		$0025(a0)
		move.w	#$12C,$002C(a0)
		move.b	#3,$002A(a0)
loc_1E3E4:
		rts
loc_1E3E6:
		subq.w	#1,$0032(a0)
		bpl.s	loc_1E402
		move.w	$0034(a0),$0032(a0)
		neg.w	$0010(a0)
		bchg	#0,$0022(a0)
		move.b	#1,$001D(a0)
loc_1E402:
		rts
loc_1E404:
		tst.b	$0021(a0)
		beq.w	loc_1E4A6
		moveq	#2,d3
loc_1E40E:
		jsrto	JmpTo8_SingleObjLoad
		bne.s	loc_1E480
		_move.b	id(a0),id(a1)
		move.b	#8,$0024(a1)
		move.w	x_pos(a0),x_pos(a1)
		move.w	y_pos(a0),y_pos(a1)
		move.l	mappings(a0),mappings(a1)
		move.w	#$24E0,art_tile(a1)
		ori.b	#4,$0001(a1)
		move.b	#3,$0018(a1)
		move.w	#$FF00,$0012(a1)
		move.b	#4,$001C(a1)
		move.b	#$78,$0021(a1)
		cmpi.w	#1,d3
		beq.s	loc_1E47A
		blt.s	loc_1E46C
		move.w	#$C0,$0010(a1)
		addi.w	#$FF40,$0012(a1)
		bra.s	loc_1E480
loc_1E46C:
		move.w	#$FF00,$0010(a1)
		addi.w	#$FFC0,$0012(a1)
		bra.s	loc_1E480
loc_1E47A:
		move.w	#$40,$0010(a1)
loc_1E480:
		dbf	d3,loc_1E40E
		jsrto	JmpTo8_SingleObjLoad
		bne.s	loc_1E4A2
		_move.b	id(a0),id(a1)
		move.b	#$A,$0024(a1)
		move.l	mappings(a0),mappings(a1)
		move.w	#$24E0,art_tile(a1)
loc_1E4A2:
	if RemoveJmpTos
JmpTo36_DeleteObject:
	endif
		jmpto	JmpTo36_DeleteObject
loc_1E4A6:
		rts