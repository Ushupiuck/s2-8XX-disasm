;===============================================================================
; Object 0x4F - Hidden Palace - Dinobot
; [ Begin ]
;===============================================================================
Obj_0x4F_Dinobot: ; loc_1DEAC:
		moveq	#0,d0
		move.b	routine(a0),d0
		move.w	loc_1DEBA(pc,d0.w),d1
		jmp	loc_1DEBA(pc,d1.w)
loc_1DEBA:
		dc.w	loc_1DEC0-loc_1DEBA
		dc.w	loc_1DF16-loc_1DEBA
		dc.w	loc_1DFB8-loc_1DEBA
loc_1DEC0:
		move.l	#Obj4F_MapUnc_1DFCA,mappings(a0) ; loc_1DFCA
		move.w	#$500,art_tile(a0)
		move.b	#4,$0001(a0)
		move.b	#4,$0018(a0)
		move.b	#$10,$0019(a0)
		move.b	#$10,$0016(a0)
		move.b	#6,$0017(a0)
		move.b	#$C,$0020(a0)
		jsrto	JmpTo3_ObjectMoveAndFall
		jsr	(ObjHitFloor).l			 ; loc_13898
		tst.w	d1
		bpl.s	loc_1DF14
		add.w	d1,y_pos(a0)
		move.w	#0,$0012(a0)
		addq.b	#2,routine(a0)
		bchg	#0,$0022(a0)
loc_1DF14:
		rts
loc_1DF16:
		moveq	#0,d0
		move.b	$0025(a0),d0
		move.w	loc_1DF5C(pc,d0.w),d1
		jsr	loc_1DF5C(pc,d1.w)
		lea	(loc_1DFBC).l,A1
		jsrto	JmpTo8_AnimateSprite
		move.w	x_pos(a0),d0
		andi.w	#$FF80,d0
		sub.w	(Camera_X_pos_coarse).w,d0
		cmpi.w	#$280,d0
		bhi.w	loc_1DF46
		jmpto	JmpTo15_DisplaySprite
loc_1DF46:
		lea	(Object_Respawn_Table).w,A2
		moveq	#0,d0
		move.b	$0023(a0),d0
		beq.s	loc_1DF58
		bclr	#7,2(a2,d0.w)
loc_1DF58:
		jmpto	JmpTo35_DeleteObject
loc_1DF5C:
		dc.w	loc_1DF60-loc_1DF5C
		dc.w	loc_1DF84-loc_1DF5C

loc_1DF60:
		subq.w	#1,$0030(a0)
		bpl.s	loc_1DF82
		addq.b	#2,$0025(a0)
		move.w	#$FF80,$0010(a0)
		move.b	#1,$001C(a0)
		bchg	#0,$0022(a0)
		bne.s	loc_1DF82
		neg.w	$0010(a0)
loc_1DF82:
		rts

loc_1DF84:
		jsrto	JmpTo13_ObjectMove
		jsr	(ObjHitFloor).l			 ; loc_13898
		cmpi.w	#$FFF8,d1
		blt.s	loc_1DFA0
		cmpi.w	#$C,d1
		bge.s	loc_1DFA0
		add.w	d1,y_pos(a0)
		rts
loc_1DFA0:
		subq.b	#2,$0025(a0)
		move.w	#$3B,$0030(a0)
		move.w	#0,$0010(a0)
		move.b	#0,$001C(a0)
		rts
loc_1DFB8:
		jmpto	JmpTo35_DeleteObject