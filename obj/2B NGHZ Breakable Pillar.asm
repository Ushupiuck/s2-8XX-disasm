;===============================================================================
; Object 0x2B - Neo Green Hill - Breakable Pillar
; [ Begin ]
;===============================================================================
Obj_0x2B_Breakable_Pillar: ; loc_19A1E:
		moveq	#0,d0
		move.b	routine(a0),d0
		move.w	loc_19A2C(pc,d0),d1
		jmp	loc_19A2C(pc,d1)
loc_19A2C:
		dc.w	loc_19A32-loc_19A2C
		dc.w	loc_19A60-loc_19A2C
		dc.w	loc_19B50-loc_19A2C
loc_19A32:
		addq.b	#2,routine(a0)
		move.l	#Breakable_Pillar_Mappings,mappings(a0) ; loc_19C30
		move.w	#$2000,art_tile(a0)
		jsrto	JmpTo14_Adjust2PArtPointer
		ori.b	#$04,$0001(a0)
		move.b	#$10,$0019(a0)
		move.b	#$18,$0016(a0)
		move.b	#$04,$0018(a0)
loc_19A60:
		move.w	x_pos(a0),-(sp)
		bsr.w	loc_19AEA
		moveq	#0,d1
		move.b	$0019(a0),d1
		addi.w	#$000B,d1
		moveq	#0,d2
		move.b	$0016(a0),d2
		move.w	d2,d3
		addq.w	#1,d3
		move.w	(sp)+,d4
		jsrto	JmpTo6_SolidObject
		move.b	$0022(a0),d0
		andi.b	#$18,d0
		bne.w	loc_19A92
		jmpto	JmpTo12_MarkObjGone
loc_19A92:
		lea	(loc_19B80).l,A4
		lea	(loc_19B72).l,A2
		addq.b	#$07,$001A(a0)
		bsr.w	loc_19BB8
		lea	(MainCharacter).w,A1
		moveq	#$03,d6
		bsr.s	loc_19AB8
		lea	(Sidekick).w,A1
		addq.b	#1,d6
		bra.w	loc_19B50
loc_19AB8:
		bclr	d6,$0022(a0)
		beq.s	loc_19AE8
		bset	#2,$0022(a1)
		move.b	#$0E,$0016(a1)
		move.b	#$07,$0017(a1)
		move.b	#2,$001C(a1)
		bset	#1,$0022(a1)
		bclr	#$03,$0022(a1)
		move.b	#2,$0024(a1)
loc_19AE8:
		rts
loc_19AEA:
		moveq	#0,d0
		move.b	$0025(a0),d0
		move.w	loc_19AF8(pc,d0),d1
		jmp	loc_19AF8(pc,d1)
loc_19AF8:
		dc.w	loc_19AFE-loc_19AF8
		dc.w	loc_19B28-loc_19AF8
		dc.w	loc_19B26-loc_19AF8
loc_19AFE:
		tst.w	(Debug_placement_mode).w
		bne.s	loc_19B26
		lea	(MainCharacter).w,A1
		bsr.s	loc_19B0E
		lea	(Sidekick).w,A1
loc_19B0E:
		move.w	x_pos(a0),d0
		sub.w	x_pos(a1),d0
		bhs.s	loc_19B1A
		neg.w	d0
loc_19B1A:
		cmpi.w	#$0040,d0
		bhs.s	loc_19B26
		move.b	#2,$0025(a0)
loc_19B26:
		rts
loc_19B28:
		subq.w	#1,$0034(a0)
		bhs.s	loc_19B4E
		move.w	#$0003,$0034(a0)
		subq.w	#$04,y_pos(a0)
		addq.b	#$04,$0016(a0)
		addq.b	#1,$001A(a0)
		cmpi.b	#$06,$001A(a0)
		bne.s	loc_19B4E
		move.b	#$04,$0025(a0)
loc_19B4E:
		rts
loc_19B50:
		tst.b	$003F(a0)
		beq.s	loc_19B5C
		subq.b	#1,$003F(a0)
		bra.s	loc_19B66
loc_19B5C:
		jsrto	JmpTo9_ObjectMove		  ; loc_1A0BC
		addi.w	#$0018,$0012(a0)
loc_19B66:
		tst.b	$0001(a0)
		bpl.w	JmpTo21_DeleteObject
		jmpto	JmpTo6_DisplaySprite

	if RemoveJmpTos
JmpTo21_DeleteObject:
		jmp	(DeleteObject).l
	endif

loc_19B72:
		dc.b	$00,$00,$00,$00,$04,$04,$08,$08,$0C,$0C,$10,$10,$14,$14
loc_19B80:
		dc.w	$FE00,$FE00,$0200,$FE00,$FE40,$FE40,$01C0,$FE40
		dc.w	$FE80,$FE80,$0180,$FE80,$FEC0,$FEC0,$0140,$FEC0
		dc.w	$FF00,$FF00,$0100,$FF00,$FF40,$FF40,$00C0,$FF40
		dc.w	$FF80,$FF80,$0080,$FF80
loc_19BB8:
		moveq	#0,d0
		move.b	$001A(a0),d0
		add.w	d0,d0
		movea.l	mappings(a0),a3
		adda.w	(a3,d0.w),a3
		move.w	(a3)+,d1
		subq.w	#1,d1
		bset	#$05,$0001(a0)
		_move.b	0(a0),d4
		move.b	$0001(a0),d5
		movea.l	a0,a1
		bra.s	loc_19BE6
loc_19BDE:
		jsrto	JmpTo4_SingleObjLoad2
		bne.s	loc_19C26
		addq.w	#$08,A3
loc_19BE6:
		move.b	#$04,$0024(a1)
		_move.b	d4,0(a1)
		move.l	A3,mappings(a1)
		move.b	d5,$0001(a1)
		move.w	x_pos(a0),x_pos(a1)
		move.w	y_pos(a0),y_pos(a1)
		move.w	art_tile(a0),art_tile(a1)
		move.b	$0018(a0),$0018(a1)
		move.b	$0019(a0),$0019(a1)
		move.w	(a4)+,$0010(a1)
		move.w	(a4)+,$0012(a1)
		move.b	(a2)+,$003F(a1)
		dbf	d1,loc_19BDE
loc_19C26:
		move.w	#SndID_SlowSmash,d0
		jmp	(PlaySound).l			  ; loc_14C6