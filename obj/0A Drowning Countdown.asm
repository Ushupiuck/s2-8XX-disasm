;===============================================================================
; Object 0x0A -
; [ Begin ]
;===============================================================================
Obj_0x0A_Bubbles_And_Numbers: ; loc_1207C:
		moveq	#0,d0
		move.b	routine(a0),d0
		move.w	loc_1208A(pc,d0),d1
		jmp	loc_1208A(pc,d1)
loc_1208A:
		dc.w	loc_1209C-loc_1208A
		dc.w	loc_120F8-loc_1208A
		dc.w	loc_12104-loc_1208A
		dc.w	loc_1216E-loc_1208A
		dc.w	JmpTo2_DeleteObject-loc_1208A ; loc_12182-...
		dc.w	loc_1230C-loc_1208A
		dc.w	loc_12188-loc_1208A
		dc.w	loc_1216E-loc_1208A
		dc.w	JmpTo2_DeleteObject-loc_1208A ; loc_12182-...
loc_1209C:
		addq.b	#2,$24(a0)
		move.l	#Obj_0x0A_Bubbles_Mappings,4(a0) ; loc_14374
		move.w	#$8500,2(a0)
		move.b	#$84,1(a0)
		move.b	#$10,$19(a0)
		move.b	#1,$18(a0)
		move.b	$28(a0),d0
		bpl.s	loc_120E4
		addq.b	#8,$24(a0)
		move.l	#Obj_0x0A_Numbers_Mappings,4(a0) ; loc_125C2
		move.w	#$440,2(a0)
		andi.w	#$7F,d0
		move.b	d0,$33(a0)
		bra.w	loc_1230C
loc_120E4:
		move.b	d0,$1C(a0)
		bsr.w	Adjust2PArtPointer	   ; loc_DC30
		move.w	x_pos(a0),$30(a0)
		move.w	#$FF78,$12(a0)
loc_120F8:
		lea	(loc_12530).l,A1
		jsr	(AnimateSprite).l			; (loc_d412)
loc_12104:
		move.w	(Water_Level_1).w,d0
		cmp.w	y_pos(a0),d0
		blo.s	loc_1212A
		move.b	#6,$24(a0)
		addq.b	#7,$1C(a0)
		cmpi.b	#$D,$1C(a0)
		beq.s	loc_1216E
		blo.s	loc_1216E
		move.b	#$D,$1C(a0)
		bra.s	loc_1216E
loc_1212A:
		tst.b	(WindTunnel_flag).w
		beq.s	loc_12134
		addq.w	#4,$30(a0)
loc_12134:
		move.b	$26(a0),d0
		addq.b	#1,$26(a0)
		andi.w	#$7F,d0
		lea	(loc_1220C).l,A1
		move.b	(A1,d0),d0
		ext.w	d0
		add.w	$30(a0),d0
		move.w	d0,x_pos(a0)
		bsr.s	loc_121C0
		jsr	(ObjectMove).l				; (loc_d27A)
		tst.b	1(a0)
		bpl.s	JmpTo_DeleteObject
		jmp	(DisplaySprite).l			; (loc_d3C2)
; loc_12168:
JmpTo_DeleteObject:
		jmp	(DeleteObject).l

loc_1216E:
		bsr.s	loc_121C0
		lea	(loc_12530).l,A1
		jsr	(AnimateSprite).l			; (loc_d412)
		jmp	(DisplaySprite).l			; (loc_d3C2)
; loc_12182:
JmpTo2_DeleteObject:
		jmp	(DeleteObject).l
loc_12188:
		cmpi.w	#$C,(Current_Air).w
		bhi.s	JmpTo3_DeleteObject
		subq.w	#1,$38(a0)
		bne.s	loc_121A2
		move.b	#$E,$24(a0)
		addq.b	#7,$1C(a0)
		bra.s	loc_1216E
loc_121A2:
		lea	(loc_12530).l,A1
		jsr	(AnimateSprite).l			; (loc_d412)
		tst.b	1(a0)
		bpl.s	JmpTo3_DeleteObject
		jmp	(DisplaySprite).l			; (loc_d3C2)
; loc_121BA:
JmpTo3_DeleteObject:
		jmp	(DeleteObject).l
loc_121C0:
		tst.w	$38(a0)
		beq.s	loc_1220A
		subq.w	#1,$38(a0)
		bne.s	loc_1220A
		cmpi.b	#7,$1C(a0)
		bhs.s	loc_1220A
		move.w	#$F,$38(a0)
		clr.w	$12(a0)
		move.b	#$80,1(a0)
		move.w	x_pos(a0),d0
		sub.w	(Camera_X_pos).w,d0
		addi.w	#$80,d0
		move.w	d0,x_pos(a0)
		move.w	y_pos(a0),d0
		sub.w	(Camera_Y_pos).w,d0
		addi.w	#$80,d0
		move.w	d0,$A(a0)
		move.b	#$C,$24(a0)
loc_1220A:
		rts

loc_1220C:
		dc.b	$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$02,$02,$02,$02,$02
		dc.b	$02,$02,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
		dc.b	$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$02
		dc.b	$02,$02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00
		dc.b	$00,$FF,$FF,$FF,$FF,$FF,$FE,$FE,$FE,$FE,$FE,$FD,$FD,$FD,$FD,$FD
		dc.b	$FD,$FD,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
		dc.b	$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FD
		dc.b	$FD,$FD,$FD,$FD,$FD,$FD,$FE,$FE,$FE,$FE,$FE,$FF,$FF,$FF,$FF,$FF
		dc.b	$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$02,$02,$02,$02,$02
		dc.b	$02,$02,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
		dc.b	$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$02
		dc.b	$02,$02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00
		dc.b	$00,$FF,$FF,$FF,$FF,$FF,$FE,$FE,$FE,$FE,$FE,$FD,$FD,$FD,$FD,$FD
		dc.b	$FD,$FD,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
		dc.b	$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FD
		dc.b	$FD,$FD,$FD,$FD,$FD,$FD,$FE,$FE,$FE,$FE,$FE,$FF,$FF,$FF,$FF,$FF
loc_1230C:
		tst.w	$2C(a0)
		bne.w	loc_123F6
		cmpi.b	#6,(MainCharacter+routine).w
		bhs.w	loc_124FC
		btst	#6,(MainCharacter+$22).w
		beq.w	loc_124FC
		subq.w	#1,$38(a0)
		bpl.w	loc_1241C
		move.w	#59,$38(a0)
		move.w	#1,$36(a0)
		jsr	(PseudoRandomNumber).l		; loc_31E4
		andi.w	#1,d0
		move.b	d0,$34(a0)
		move.w	(Current_Air).w,d0
		cmpi.w	#$19,d0
		beq.s	loc_12386
		cmpi.w	#$14,d0
		beq.s	loc_12386
		cmpi.w	#$F,d0
		beq.s	loc_12386
		cmpi.w	#$C,d0
		bhi.s	loc_12390
		bne.s	loc_12372
		move.w	#MusID_LevelSel,d0
		jsr	(PlayMusic).l			 ; loc_14C0
loc_12372:
		subq.b	#1,$32(a0)
		bpl.s	loc_12390
		move.b	$33(a0),$32(a0)
		bset	#7,$36(a0)
		bra.s	loc_12390
loc_12386:
		move.w	#SndID_WaterWarning,d0
		jsr	(PlaySound).l			  ; loc_14C6
loc_12390:
		subq.w	#1,(Current_Air).w
		bhs.w	loc_1241A
		bsr.w	ResumeMusic				; loc_124FE
		move.b	#$81,(MainCharacter+$2A).w
		move.w	#SndID_Drown,d0
		jsr	(PlaySound).l			  ; loc_14C6
		move.b	#$A,$34(a0)
		move.w	#1,$36(a0)
		move.w	#$78,$2C(a0)
		move.l	a0,-(sp)
		lea	(MainCharacter).w,a0
		bsr.w	Sonic_ResetOnFloor		; loc_1090C
		move.b	#$17,$1C(a0)
		bset	#1,$22(a0)
		bset	#7,2(a0)
		move.w	#0,$12(a0)
		move.w	#0,$10(a0)
		move.w	#0,$14(a0)
		move.b	#1,(Deform_lock).w
		movea.l	(sp)+,a0
		rts
loc_123F6:
		subq.w	#1,$2C(a0)
		bne.s	loc_12404
		move.b	#6,(MainCharacter+routine).w
		rts
loc_12404:
		move.l	A0,-(sp)
		lea	(MainCharacter).w,A0
		jsr	(ObjectMove).l				; (loc_d27A)
		addi.w	#$10,$12(a0)
		move.l	(sp)+,A0
		bra.s	loc_1241C
loc_1241A:
		bra.s	loc_1242C
loc_1241C:
		tst.w	$36(a0)
		beq.w	loc_124FC
		subq.w	#1,$3A(a0)
		bpl.w	loc_124FC
loc_1242C:
		jsr	(PseudoRandomNumber).l		; loc_31E4
		andi.w	#$F,d0
		move.w	d0,$3A(a0)
		jsr	(SingleObjLoad).l		 ; (loc_E772)
		bne.w	loc_124FC
		_move.b	#id_Obj0A,id(a1)
		move.w	(MainCharacter+x_pos).w,x_pos(a1)
		moveq	#6,d0
		btst	#0,(MainCharacter+$22).w
		beq.s	loc_12462
		neg.w	d0
		move.b	#$40,$26(a1)
loc_12462:
		add.w	d0,x_pos(a1)
		move.w	(MainCharacter+$C).w,y_pos(a1)
		move.b	#6,$28(a1)
		tst.w	$2C(a0)
		beq.w	loc_124AE
		andi.w	#7,$3A(a0)
		addi.w	#0,$3A(a0)
		move.w	(MainCharacter+$C).w,d0
		subi.w	#$C,d0
		move.w	d0,y_pos(a1)
		jsr	(PseudoRandomNumber).l		; loc_31E4
		move.b	d0,$26(a1)
		move.w	(Timer_frames).w,d0
		andi.b	#3,d0
		bne.s	loc_124F2
		move.b	#$E,$28(a1)
		bra.s	loc_124F2
loc_124AE:
		btst	#$07,$0036(a0)
		beq.s	loc_124F2
		move.w	(Current_Air).w,d2
		lsr.w	#1,d2
		jsr	(PseudoRandomNumber).l		; loc_31E4
		andi.w	#3,d0
		bne.s	loc_124DA
		bset	#6,$36(a0)
		bne.s	loc_124F2
		move.b	d2,$28(a1)
		move.w	#$1C,$38(a1)
loc_124DA:
		tst.b	$34(a0)
		bne.s	loc_124F2
		bset	#6,$36(a0)
		bne.s	loc_124F2
		move.b	d2,$28(a1)
		move.w	#$1C,$38(a1)
loc_124F2:
		subq.b	#1,$34(a0)
		bpl.s	loc_124FC
		clr.w	$36(a0)
loc_124FC:
		rts