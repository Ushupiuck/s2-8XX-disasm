; ===========================================================================
; ---------------------------------------------------------------------------
; Object 07 - Sinking Oil from OOZ
; ---------------------------------------------------------------------------
; Sprite_180d0: Obj_0x07:
Obj07:
		moveq	#0,d0
		move.b	routine(a0),d0
		move.w	Obj07_Index(pc,d0.w),d1
		jmp	Obj07_Index(pc,d1.w)
; ===========================================================================
; off_180DE:
Obj07_Index:	offsetTable
		offsetTableEntry.w Obj07_Init
		offsetTableEntry.w Obj07_Main
; ===========================================================================
; loc_180E2:
Obj07_Init:
		addq.b	#2,routine(a0)
		move.w	#$758,y_pos(a0)
		move.b	#$20,$19(a0)
		move.w	y_pos(a0),$30(a0)
		move.b	#$30,$38(a0)
		bset	#7,$22(a0)
; loc_18104:
Obj07_Main:
		; check player 1
		lea	(MainCharacter).w,a1
		moveq	#8,d1
		move.b	$22(a0),d0
		and.b	d1,d0
		bne.s	Obj07_CheckKillChar1
		cmpi.b	#$30,$38(a0)
		beq.s	Obj07_CheckSupportChar1
		addq.b	#1,$38(a0)
		bra.s	Obj07_CheckSupportChar1
; ---------------------------------------------------------------------------
; loc_18120:
Obj07_CheckKillChar1:
		tst.b	$38(a0)
		beq.s	Obj07_SuffocateCharacter
		subq.b	#1,$38(a0)
; loc_1812A:
Obj07_CheckSupportChar1:
		moveq	#$20,d1
		moveq	#0,d3
		move.b	$38(a0),d3
		moveq	#3,d6
		move.w	x_pos(a1),d4
		move.w	d4,x_pos(a0)
		jsrto	JmpTo_PlatformObject_SingleCharacter	; stop the character from falling past the oil

		; check 2
		lea	(Sidekick).w,a1
		moveq	#$10,d1
		move.b	$22(a0),d0
		and.b	d1,d0
		bne.s	Obj07_CheckKillChar2
		cmpi.b	#$30,$3A(a0)
		beq.s	Obj07_CheckSupportChar2
		addq.b	#1,$3A(a0)
		bra.s	Obj07_CheckSupportChar2
; ---------------------------------------------------------------------------
; loc_1815C:
Obj07_CheckKillChar2:
		tst.b	$3A(a0)
		beq.s	Obj07_SuffocateCharacter
		subq.b	#1,$3A(a0)
; loc_18166:
Obj07_CheckSupportChar2:
		moveq	#$20,d1
		moveq	#0,d3
		move.b	$3A(a0),d3
		moveq	#4,d6
		move.w	x_pos(a1),d4
		move.w	d4,x_pos(a0)
		jsrto	JmpTo_PlatformObject_SingleCharacter	; stop the character from falling past the oil
		rts
; ---------------------------------------------------------------------------
; loc_1817E:
Obj07_SuffocateCharacter:
		not.b	d1
		and.b	d1,$22(a0)
		move.l	a0,-(sp)
		movea.l	a0,a2
		movea.l	a1,a0
		jsrto	JmpTo3_KillCharacter
		movea.l	(sp)+,a0
		rts
; ===========================================================================