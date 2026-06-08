; ===========================================================================
; ---------------------------------------------------------------------------
; Object 01 - Sonic
; ---------------------------------------------------------------------------
; Sprite_FC48: Obj_0x01_Sonic:
Obj01:
		tst.w	(Debug_placement_mode).w; is Debug Mode being used?
		beq.s	Obj01_Normal		; if not,branch
		jmp	(DebugMode).l
; ---------------------------------------------------------------------------
; loc_FC54: Sonic_Normal:
Obj01_Normal:
		moveq	#0,d0
		move.b	routine(a0),d0
		move.w	Obj01_Index(pc,d0.w),d1
		jmp	Obj01_Index(pc,d1.w)
; ===========================================================================
; loc_FC62: Sonic_Index:
Obj01_Index:	offsetTable
		offsetTableEntry.w Obj01_Init
		offsetTableEntry.w Obj01_Control
		offsetTableEntry.w Obj01_Hurt
		offsetTableEntry.w Obj01_Dead
		offsetTableEntry.w Obj01_Gone
; ===========================================================================
; loc_FC6C: Sonic_Main:
Obj01_Init:
		addq.b	#2,routine(a0)	; => Obj01_Control
		move.b	#$13,$16(a0)	; this sets Sonic's collision height (2*pixels)
		move.b	#9,$17(a0)
		move.l	#MapUnc_Sonic,4(a0)
		move.w	#$780,2(a0)
		bsr.w	Adjust2PArtPointer
		move.b	#2,$18(a0)
		move.b	#$18,$19(a0)
		move.b	#4,1(a0)
		move.w	#$600,(Sonic_top_speed).w	; set Sonic's top speed
		move.w	#$C,(Sonic_acceleration).w	; set Sonic's acceleration
		move.w	#$80,(Sonic_deceleration).w	; set Sonic's deceleration
		move.b	#$C,$3E(a0)
		move.b	#$D,$3F(a0)
		move.b	#0,$2C(a0)
		move.b	#4,$2D(a0)
		move.w	#0,(Sonic_Pos_Record_Index).w
		move.w	#$3F,d2

loc_FCd4:
		bsr.w	Sonic_RecordPos
		move.w	#0,(a1,d0.w)
		dbf	d2,loc_FCd4

; ---------------------------------------------------------------------------
; Normal state for Sonic
; ---------------------------------------------------------------------------
; loc_FCE2: Sonic_Control:
Obj01_Control:
		tst.w	(Debug_mode_flag).w		; is Debug Mode enabled?
		beq.s	loc_FCFC			; if not,branch
		btst	#button_B,(Ctrl_1_Press).w		; is button B pressed?
		beq.s	loc_FCFC			; if not,branch
		move.w	#1,(Debug_placement_mode).w	; change Sonic into a ring/item
		clr.b	(Control_Locked).w		; unlock control
		rts
; -----------------------------------------------------------------------

loc_FCFC:
		tst.b	(Control_Locked).w	; are controls locked?
		bne.s	loc_Fd08		; if yes,branch
		move.w	(Ctrl_1).w,(Ctrl_1_Logical).w	; copy new held buttons to enable joypad control

loc_Fd08:
		btst	#0,$2A(a0)		; is Sonic interacting with another object that holds him in place or controls his movement somehow?
		bne.s	Obj01_ControlsLock	; if yes,branch
		moveq	#0,d0
		move.b	$22(a0),d0
		andi.w	#6,d0
		move.w	Obj01_Modes(pc,d0.w),d1
		jsr	Obj01_Modes(pc,d1.w)	; run Sonic's movement code
; loc_Fd22: Sonic_ControlsLock:
Obj01_ControlsLock:
		bsr.s	Sonic_Display
		bsr.w	Sonic_RecordPos
		bsr.w	Sonic_Water
		move.b	(Primary_Angle).w,$36(a0)
		move.b	(Secondary_Angle).w,$37(a0)
		tst.b	(WindTunnel_flag).w
		beq.s	loc_Fd4A
		tst.b	$1C(a0)
		bne.s	loc_Fd4A
		move.b	$1D(a0),$1C(a0)

loc_Fd4A:
		bsr.w	Sonic_Animate
		tst.b	$2A(a0)
		bmi.s	loc_Fd5A
		jsr	(TouchResponse).l

loc_Fd5A:
		bra.w	LoadSonicDynPLC
; ===========================================================================
; loc_Fd5E:
Obj01_Modes:	offsetTable
		offsetTableEntry.w Obj01_MdNormal	; 0 - not airborne or rolling
		offsetTableEntry.w Obj01_MdAir		; 2 - airborne
		offsetTableEntry.w Obj01_MdRoll		; 4 - rolling
		offsetTableEntry.w Obj01_MdJump		; 6 - jumping
; ===========================================================================
; byte_Fd66:
Sonic_MusicList:	zoneOrderedTable 1,1
	zoneTableEntry.b	MusID_GHZ	; GHZ
	zoneTableEntry.b	MusID_GHZ	; OWZ
	zoneTableEntry.b	MusID_MTZ	; WZ
	zoneTableEntry.b	MusID_SSZ	; SSZ
	zoneTableEntry.b	MusID_MTZ	; MTZ
	zoneTableEntry.b	MusID_MTZ	; MTZ2
	zoneTableEntry.b	MusID_BOZ	; BLZ
	zoneTableEntry.b	MusID_HTZ	; HTZ
	zoneTableEntry.b	MusID_HPZ	; HPZ
	zoneTableEntry.b	MusID_RWZ	; RWZ
	zoneTableEntry.b	MusID_OOZ	; OOZ
	zoneTableEntry.b	MusID_DHZ	; DHZ
	zoneTableEntry.b	MusID_CNZ	; CNZ
	zoneTableEntry.b	MusID_CPZ	; CPZ
	zoneTableEntry.b	MusID_CPZ	; GCZ
	zoneTableEntry.b	MusID_NGHZ	; NGHZ
	if FixBugs
	zoneTableEntry.b	MusID_DEZ	; DEZ
	else
	; no *proper* entry for DEZ,so it instead uses the alignment to play sound $08
	endif
	zoneTableEnd
	even

; ===========================================================================

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_Fd76:
Sonic_Display:
		move.w	$30(a0),d0
		beq.s	Obj01_Display
		subq.w	#1,$30(a0)
		lsr.w	#3,d0
		bhs.s	Obj01_ChkInvin
; loc_FD84:
Obj01_Display:
		jsr	(DisplaySprite).l
; loc_FD8A:
Obj01_ChkInvin:	; Checks if Sonic has run out of invincibility frames
		tst.b	(Invincibility_flag).w
		beq.s	Obj01_ChkShoes
		tst.w	$32(a0)
		beq.s	Obj01_ChkShoes
		subq.w	#1,$32(a0)
		bne.s	Obj01_ChkShoes
		tst.b	(Current_Boss_ID).w
		bne.s	Obj01_RmvInvin
		cmpi.w	#$C,(Current_Air).w
		blo.s	Obj01_RmvInvin
		moveq	#0,d0
		move.b	(Current_Zone).w,d0
		lea	Sonic_MusicList(pc),a1
		move.b	(a1,d0.w),d0
		jsr	(PlayMusic).l
; loc_FDBE:
Obj01_RmvInvin:
		move.b	#0,(Invincibility_flag).w
; loc_FDC4:
Obj01_ChkShoes:	; Checks if Sonic should still have the speed shoes
		tst.b	(Speed_shoes).w
		beq.s	Obj01_ExitChk
		tst.w	$34(a0)
		beq.s	Obj01_ExitChk
		subq.w	#1,$34(a0)
		bne.s	Obj01_ExitChk
		move.w	#$600,(Sonic_top_speed).w
		move.w	#$C,(Sonic_acceleration).w
		move.w	#$80,(Sonic_deceleration).w
		move.b	#0,(Speed_shoes).w
		move.w	#MusID_SlowDown,d0	; restore music tempo
		jmp	(PlayMusic).l
; return_FDF8:
Obj01_ExitChk:
		rts
; End of subroutine Sonic_Display

; ---------------------------------------------------------------------------
; Subroutine to record Sonic's previous positions for invincibility stars
; and input/status flags for Tails' AI to follow
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_FDFA: CopySonicMovesForTails:
Sonic_RecordPos:
		move.w	(Sonic_Pos_Record_Index).w,d0
		lea	(Sonic_Pos_Record_Buf).w,a1
		lea	(a1,d0.w),a1
		move.w	x_pos(a0),(a1)+
		move.w	y_pos(a0),(a1)+
		addq.b	#4,(Sonic_Pos_Record_Index+1).w
		lea	(Sonic_Stat_Record_Buf).w,a1
		move.w	(Ctrl_1).w,(a1,d0.w)
		rts
; End of function Sonic_RecordPos

; ---------------------------------------------------------------------------
; Seemingly an earlier subroutine to copy Sonic's status flags for Tails' AI,
; also present in the Nick Arcade prototype
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_FE1E: Unused_RecordPos:
		move.w	(unk_EEE0).w,d0
		subq.b	#4,d0
		lea	(unk_E600).w,a1
		lea	(a1,d0.w),a2
		move.w	x_pos(a0),d1
		swap	d1
		move.w	y_pos(a0),d1
		cmp.l	(a2),d1
		beq.s	return_FE4C
		addq.b	#4,d0
		lea	(a1,d0.w),a2
		move.w	x_pos(a0),(a2)+
		move.w	y_pos(a0),(a2)
		addq.b	#4,(unk_EEE0+1).w

return_FE4C:
		rts
; End of function Unused_RecordPos

; ---------------------------------------------------------------------------
; Subroutine for Sonic when he's underwater
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_FE4E:
Sonic_Water:
		tst.b	(Water_flag).w		; is this a water level?
		bne.s	Obj01_InWater		; if not,branch

return_FE54:
		rts
; ---------------------------------------------------------------------------
; loc_FE56: Sonic_InLevelWithWater:
Obj01_InWater:
		move.w	(Water_Level_1).w,d0
		cmp.w	y_pos(a0),d0		; is Sonic underwater?
		bge.s	Obj01_OutWater		; if not,branch

		bset	#6,$22(a0)		; set underwater flag
		bne.s	return_FE54		; if already underwater,branch

		bsr.w	ResumeMusic
		move.b	#$A,(BreathingBubbles).w	; load Obj0A (Sonic's breathing bubbles) at $FFFFB340
		move.b	#$81,(BreathingBubbles+$28).w
		move.w	#$300,(Sonic_top_speed).w
		move.w	#6,(Sonic_acceleration).w
		move.w	#$40,(Sonic_deceleration).w
		asr.w	$10(a0)
		asr.w	$12(a0)			; memory operands can only be shifted one bit at a time
		asr.w	$12(a0)
		beq.s	return_FE54
		move.b	#8,(WaterSplash).w	; load Obj08 (splash animation) at $FFFFB300
		move.w	#SndID_Splash,d0			; splash sound
		jmp	(PlaySound).l
; ---------------------------------------------------------------------------
; loc_FEA8: Sonic_NotInWater:
Obj01_OutWater:
		bclr	#6,$22(a0)	; clear underwater flag
		beq.s	return_FE54	; if already cleared,branch
		bsr.w	ResumeMusic
		move.w	#$600,(Sonic_top_speed).w
		move.w	#$C,(Sonic_acceleration).w
		move.w	#$80,(Sonic_deceleration).w
		asl.w	$12(a0)
		beq.w	return_FE54
		move.b	#8,(WaterSplash).w	; load Obj08 (splash animation) at $FFFFB300
		cmpi.w	#-$1000,$12(a0)
		bgt.s	loc_FEE2
		move.w	#-$1000,$12(a0)		; limit upwards y-velocity when exiting out of water

loc_FEE2:
		move.w	#SndID_Splash,d0			; splash sound
		jmp	(PlaySound).l
; End of subroutine Sonic_Water

; ===========================================================================
; ---------------------------------------------------------------------------
; Start of subroutine Obj01_MdNormal
; Called if Sonic is neither airborne nor rolling this frame
; ---------------------------------------------------------------------------
; loc_FEEC: Sonic_MdNormal:
Obj01_MdNormal:
		bsr.w	Sonic_CheckSpindash
		bsr.w	Sonic_Jump
		bsr.w	Sonic_SlopeResist
		bsr.w	Sonic_Move
		bsr.w	Sonic_Roll
		bsr.w	Sonic_LevelBound
		jsr	(ObjectMove).l
		bsr.w	AnglePos
		bsr.w	Sonic_SlopeRepel
		rts
; End of subroutine Obj01_MdNormal

; ===========================================================================
; Start of subroutine Obj01_MdAir
; Called if Sonic is airborne,but not in a ball (thus,probably not jumping)
; loc_FF14: Sonic_MdJump
Obj01_MdAir:
		bsr.w	Sonic_JumpHeight
		bsr.w	Sonic_ChgJumpDir
		bsr.w	Sonic_LevelBound
		jsr	(ObjectMoveAndFall).l
		btst	#6,$22(a0)	; is Sonic underwater?
		beq.s	loc_FF34	; if not,branch
		subi.w	#$28,$12(a0)	; reduce gravity by $28 ($38-$28=$10)

loc_FF34:
		bsr.w	Sonic_JumpAngle
		bsr.w	Sonic_DoLevelCollision
		rts
; End of subroutine Obj01_MdAir

; ===========================================================================
; Start of subroutine Obj01_MdRoll
; Called if Sonic is in a ball,but not airborne (thus,probably rolling)
; loc_FF3E: Sonic_MdRoll:
Obj01_MdRoll:
		bsr.w	Sonic_Jump
		bsr.w	Sonic_RollRepel
		bsr.w	Sonic_RollSpeed
		bsr.w	Sonic_LevelBound
		jsr	(ObjectMove).l
		bsr.w	AnglePos
		bsr.w	Sonic_SlopeRepel
		rts
; End of subroutine Obj01_MdRoll

; ===========================================================================
; Start of subroutine Obj01_MdJump (an Obj01_MdAir clone)
; Called if Sonic is in a ball and airborne (he could be jumping but not necessarily)
; loc_FF5E: Sonic_MdJump2:
Obj01_MdJump:
		bsr.w	Sonic_JumpHeight
		bsr.w	Sonic_ChgJumpDir
		bsr.w	Sonic_LevelBound
		jsr	(ObjectMoveAndFall).l
		btst	#6,$22(a0)	; is Sonic underwater?
		beq.s	loc_FF7E	; if not,branch
		subi.w	#$28,$12(a0)	; reduce gravity by $28 ($38-$28=$10)

loc_FF7E:
		bsr.w	Sonic_JumpAngle
		bsr.w	Sonic_DoLevelCollision
		rts
; End of subroutine Obj01_MdJump

; ---------------------------------------------------------------------------
; Subroutine to make Sonic walk/run
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_FF88:
Sonic_Move:
		move.w	(Sonic_top_speed).w,d6
		move.w	(Sonic_acceleration).w,d5
		move.w	(Sonic_deceleration).w,d4

		tst.b	(Sliding_flag).w		; is Sonic sliding?
		bne.w	Obj01_Traction			; if yes,branch
		tst.w	$2E(a0)				; is Sonic's controls locked?
		bne.w	Obj01_UpdateSpeedOnGround	; if yes,branch
		btst	#button_left,(Ctrl_1_Held_Logical).w	; is left being pressed?
		beq.s	Obj01_NotLeft			; if not,branch
		bsr.w	Sonic_MoveLeft
; loc_FFB0:
Obj01_NotLeft:
		btst	#button_right,(Ctrl_1_Held_Logical).w	; is right being pressed?
		beq.s	Obj01_NotRight		; if not,branch
		bsr.w	Sonic_MoveRight
; loc_FFBC:
Obj01_NotRight:
		move.b	$26(a0),d0
		addi.b	#$20,d0
		andi.b	#$C0,d0				; is Sonic on a slope?
		bne.w	Obj01_UpdateSpeedOnGround	; if yes,branch
		tst.w	$14(a0)				; is Sonic moving?
		bne.w	Obj01_UpdateSpeedOnGround	; if yes,branch
		bclr	#5,$22(a0)
		move.b	#5,$1C(a0)	; use "standing" animation
		; check how close/far Sonic is from the edge
		btst	#3,$22(a0)	; is Sonic on the edge?
		beq.s	Sonic_Balance	; if yes,branch
		moveq	#0,d0
		move.b	$3D(a0),d0
		lsl.w	#6,d0
		lea	(MainCharacter).w,a1
		lea	(a1,d0.w),a1
		tst.b	$22(a1)
		bmi.s	Sonic_LookUp
		moveq	#0,d1
		move.b	$19(a1),d1
		move.w	d1,d2
		add.w	d2,d2
		subq.w	#4,d2
		add.w	x_pos(a0),d1
		sub.w	x_pos(a1),d1
		cmpi.w	#4,d1
		blt.s	Sonic_BalanceOnObjLeft
		cmp.w	d2,d1
		bge.s	Sonic_BalanceOnObjRight
		bra.s	Sonic_LookUp
; ===========================================================================
; loc_1001E:
Sonic_Balance:
		jsr	(ChkFloorEdge).l
		cmpi.w	#$C,d1
		blt.s	Sonic_LookUp
		cmpi.b	#3,$36(a0)
		bne.s	loc_1003A
; loc_10032:
Sonic_BalanceOnObjRight:
		bclr	#0,$22(a0)
		bra.s	loc_10048

loc_1003A:
		cmpi.b	#3,$37(a0)
		bne.s	Sonic_LookUp
; loc_10042:
Sonic_BalanceOnObjLeft:
		bset	#0,$22(a0)

loc_10048:
		move.b	#6,$1C(a0)
		bra.s	Obj01_UpdateSpeedOnGround
; ===========================================================================
; loc_10050:
Sonic_LookUp:
		btst	#button_up,(Ctrl_1_Held_Logical).w	; is up being pressed?
		beq.s	Sonic_Duck		; if not,branch
		move.b	#7,$1C(a0)		; use "looking up" animation
		bra.s	Obj01_UpdateSpeedOnGround
; ===========================================================================
; loc_10060:
Sonic_Duck:
		btst	#button_down,(Ctrl_1_Held_Logical).w	; is down being pressed?
		beq.s	Obj01_UpdateSpeedOnGround	; if not,branch
		move.b	#8,$1C(a0)		; use "ducking" animation
; ===========================================================================
; ---------------------------------------------------------------------------
; updates Sonic's speed on the ground
; ---------------------------------------------------------------------------
; sub_1006E:
Obj01_UpdateSpeedOnGround:
		move.b	(Ctrl_1_Held_Logical).w,d0
		andi.b	#button_left_mask+button_right_mask,d0
		bne.s	Obj01_Traction
		move.w	$14(a0),d0
		beq.s	Obj01_Traction
		bmi.s	Obj01_SettleLeft

; slow down when facing right and not pressing a direction
; Obj01_SettleRight:
		sub.w	d5,d0
		bhs.s	loc_10088
		move.w	#0,d0

loc_10088:
		move.w	d0,$14(a0)
		bra.s	Obj01_Traction
; ---------------------------------------------------------------------------
; slow down when facing left and not pressing a direction
; loc_1008E:
Obj01_SettleLeft:
		add.w	d5,d0
		bhs.s	loc_10096
		move.w	#0,d0

loc_10096:
		move.w	d0,$14(a0)

; increase or decrease speed on the ground
; loc_1009A:
Obj01_Traction:
		move.b	$26(a0),d0
		jsr	(CalcSine).l
		muls.w	$14(a0),d1
		asr.l	#8,d1
		move.w	d1,$10(a0)
		muls.w	$14(a0),d0
		asr.l	#8,d0
		move.w	d0,$12(a0)

; stops Sonic from running through walls that meet the ground
; loc_100B8:
Obj01_CheckWallsOnGround:
		move.b	$26(a0),d0
		addi.b	#$40,d0
		bmi.s	loc_10128
		move.b	#$40,d1		; rotate 90 degress clockwise
		tst.w	$14(a0)		; is Sonic moving?
		beq.s	loc_10128	; if not,branch
		bmi.s	loc_100d0	; if negative,branch
		neg.w	d1		; rotate COUNTER-clockwise

loc_100d0:
		move.b	$26(a0),d0
		add.b	d1,d0
		move.w	d0,-(sp)
		bsr.w	Sonic_WalkSpeed
		move.w	(sp)+,d0
		tst.w	d1
		bpl.s	loc_10128
		asl.w	#8,d1
		addi.b	#$20,d0
		andi.b	#$C0,d0
		beq.s	loc_10124
		cmpi.b	#$40,d0
		beq.s	loc_10112
		cmpi.b	#$80,d0
		beq.s	loc_1010C
		add.w	d1,$10(a0)
		bset	#5,$22(a0)
		move.w	#0,$14(a0)
		rts
; ---------------------------------------------------------------------------

loc_1010C:
		sub.w	d1,$12(a0)
		rts
; ---------------------------------------------------------------------------

loc_10112:
		sub.w	d1,$10(a0)
		bset	#5,$22(a0)
		move.w	#0,$14(a0)
		rts
; ---------------------------------------------------------------------------

loc_10124:
		add.w	d1,$12(a0)
; ---------------------------------------------------------------------------

loc_10128:
		rts
; End of subroutine Sonic_Move


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_1012A:
Sonic_MoveLeft:
		move.w	$14(a0),d0
		beq.s	loc_10132	; is Sonic starting to move to the right?
		bpl.s	Sonic_TurnLeft	; if not,branch

loc_10132:
		bset	#0,$22(a0)
		bne.s	loc_10146
		bclr	#5,$22(a0)
		move.b	#1,$1D(a0)	; force walking animation to restart if it's already in-progress

loc_10146:
		sub.w	d5,d0		; add acceleration to the left
		move.w	d6,d1
		neg.w	d1
		cmp.w	d1,d0		; compare new speed with top speed
		bgt.s	loc_10158	; if new speed is less than the maximum,branch
		add.w	d5,d0		; remove this frame's acceleration change
		cmp.w	d1,d0		; compare speed with top speed
		ble.s	loc_10158	; if speed was already greater than the maximum,branch
		move.w	d1,d0		; limit speed on ground going left

loc_10158:
		move.w	d0,$14(a0)
		move.b	#0,$1C(a0)	; use walking animation
		rts
; ---------------------------------------------------------------------------
; loc_10164:
Sonic_TurnLeft:
		sub.w	d4,d0
		bhs.s	loc_1016C
		move.w	#-$80,d0

loc_1016C:
		move.w	d0,$14(a0)
	if FixBugs
		move.b	$26(a0),d1
		addi.b	#$20,d1
		andi.b	#$C0,d1
	else
		move.b	$26(a0),d0
		addi.b	#$20,d0
		andi.b	#$C0,d0
	endif
		bne.s	return_1019A
		cmpi.w	#$400,d0
		blt.s	return_1019A
		move.b	#$D,$1C(a0)	; use "stopping" animation
		bclr	#0,$22(a0)
		move.w	#SndID_Skidding,d0
		jsr	(PlaySound).l

return_1019A:
		rts
; End of subroutine Sonic_MoveLeft


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_1019C:
Sonic_MoveRight:
		move.w	$14(a0),d0
		bmi.s	Sonic_TurnRight	; if Sonic is already moving to the left,branch
		bclr	#0,$22(a0)
		beq.s	loc_101B6
		bclr	#5,$22(a0)
		move.b	#1,$1D(a0)	; force walking animation to restart if it's already in-progress

loc_101B6:
		add.w	d5,d0		; add acceleration to the right
		cmp.w	d6,d0		; compare new speed with top speed
		blt.s	loc_101C4	; if new speed is less than the maximum,branch
		sub.w	d5,d0		; remove this frame's acceleration change
		cmp.w	d6,d0		; compare speed with top speed
		bge.s	loc_101C4	; if speed was already greater than the maximum,branch
		move.w	d6,d0		; limit speed on ground going right

loc_101C4:
		move.w	d0,$14(a0)
		move.b	#0,$1C(a0)	; use walking animation
		rts
; ---------------------------------------------------------------------------
; loc_101d0:
Sonic_TurnRight:
		add.w	d4,d0
		bhs.s	loc_101D8
		move.w	#$80,d0

loc_101D8:
		move.w	d0,$14(a0)
	if FixBugs
		move.b	$26(a0),d1
		addi.b	#$20,d1
		andi.b	#$C0,d1
	else
		move.b	$26(a0),d0
		addi.b	#$20,d0
		andi.b	#$C0,d0
	endif
		bne.s	return_10206
		cmpi.w	#-$400,d0
		bgt.s	return_10206
		move.b	#$D,$1C(a0)	; use "stopping" animation
		bset	#0,$22(a0)
		move.w	#SndID_Skidding,d0
		jsr	(PlaySound).l

return_10206:
		rts
; End of subroutine Sonic_MoveRight

; ---------------------------------------------------------------------------
; Subroutine to change Sonic's speed as he rolls
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10208:
Sonic_RollSpeed:
		move.w	(Sonic_top_speed).w,d6
		asl.w	#1,d6
		move.w	(Sonic_acceleration).w,d5
		asr.w	#1,d5	; natural roll deceleration = 1/2 normal acceleration
		; These two lines are unchanged from Sonic 1,the final would replace
		; them with "move.w #$20,d4",which made Sonic decelerate much faster
		; underwater,but they forgot to apply the change to Tails
		move.w	(Sonic_deceleration).w,d4
		asr.w	#2,d4
		tst.b	(Sliding_flag).w
		bne.w	Sonic_SetRollSpeeds
		tst.w	$2E(a0)
		bne.s	Sonic_ApplyRollSpeed
		btst	#button_left,(Ctrl_1_Held_Logical).w	; is left being pressed?
		beq.s	loc_10234		; if not,branch
		bsr.w	Sonic_RollLeft

loc_10234:
		btst	#button_right,(Ctrl_1_Held_Logical).w	; is right being pressed?
		beq.s	Sonic_ApplyRollSpeed	; if not,branch
		bsr.w	Sonic_RollRight
; loc_10240:
Sonic_ApplyRollSpeed:
		move.w	$14(a0),d0
		beq.s	Sonic_CheckRollStop
		bmi.s	Sonic_ApplyRollSpeedLeft

; Sonic_ApplyRollSpeedRight:
		sub.w	d5,d0
		bhs.s	loc_10250
		move.w	#0,d0

loc_10250:
		move.w	d0,$14(a0)
		bra.s	Sonic_CheckRollStop
; ---------------------------------------------------------------------------
; loc_10256:
Sonic_ApplyRollSpeedLeft:
		add.w	d5,d0
		bhs.s	loc_1025E
		move.w	#0,d0

loc_1025E:
		move.w	d0,$14(a0)
; loc_10262:
Sonic_CheckRollStop:
		tst.w	$14(a0)
		bne.s	Sonic_SetRollSpeeds
		bclr	#2,$22(a0)
		move.b	#$13,$16(a0)
		move.b	#9,$17(a0)
		move.b	#5,$1C(a0)
		subq.w	#5,y_pos(a0)
; ---------------------------------------------------------------------------
; loc_10284:
Sonic_SetRollSpeeds:
		move.b	$26(a0),d0
		jsr	(CalcSine).l
		muls.w	$14(a0),d0
		asr.l	#8,d0
		move.w	d0,$12(a0)	; set y velocity based on $14 and angle
		muls.w	$14(a0),d1
		asr.l	#8,d1
		cmpi.w	#$1000,d1
		ble.s	loc_102A8
		move.w	#$1000,d1	; limit Sonic's speed rolling right

loc_102A8:
		cmpi.w	#-$1000,d1
		bge.s	loc_102B2
		move.w	#-$1000,d1	; limit Sonic's speed rolling left

loc_102B2:
		move.w	d1,$10(a0)	; set x velocity based on $14 and angle
		bra.w	Obj01_CheckWallsOnGround
; End of function Sonic_RollSpeed


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_102BA:
Sonic_RollLeft:
		move.w	$14(a0),d0
		beq.s	+
		bpl.s	Sonic_BrakeRollingRight
+
		bset	#0,$22(a0)
		move.b	#2,$1C(a0)	; use "rolling" animation
		rts
; ---------------------------------------------------------------------------
; loc_102d0:
Sonic_BrakeRollingRight:
		sub.w	d4,d0		; reduce rightward rolling speed
		bhs.s	loc_102D8
		move.w	#-$80,d0

loc_102D8:
		move.w	d0,$14(a0)
		rts
; End of function Sonic_RollLeft


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_102DE:
Sonic_RollRight:
		move.w	$14(a0),d0
		bmi.s	Sonic_BrakeRollingLeft
		bclr	#0,$22(a0)
		move.b	#2,$1C(a0)	; use "rolling" animation
		rts
; ---------------------------------------------------------------------------
; loc_102F2:
Sonic_BrakeRollingLeft:
		add.w	d4,d0		; reduce leftward rolling speed
		bhs.s	+
		move.w	#$80,d0
+
		move.w	d0,$14(a0)
		rts
; End of subroutine Sonic_RollRight

; ---------------------------------------------------------------------------
; Subroutine for moving Sonic left or right when he's in the air
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10300:
Sonic_ChgJumpDir:
		move.w	(Sonic_top_speed).w,d6
		move.w	(Sonic_acceleration).w,d5
		asl.w	#1,d5
		btst	#4,$22(a0)		; did Sonic jump from rolling?
		bne.s	Obj01_Jump_ResetScr	; if yes,branch to skip midair control
		move.w	$10(a0),d0
		btst	#button_left,(Ctrl_1_Held_Logical).w
		beq.s	+	; if not holding left,branch

		bset	#0,$22(a0)
		sub.w	d5,d0	; add acceleration to the left
		move.w	d6,d1
		neg.w	d1
		cmp.w	d1,d0	; compare new speed with top speed
		bgt.s	+	; if new speed is less than the maximum,branch
		move.w	d1,d0	; limit speed in air going left,even if Sonic was already going faster (speed limit/cap)
+
		btst	#button_right,(Ctrl_1_Held_Logical).w
		beq.s	+	; if not holding right,branch

		bclr	#0,$22(a0)
		add.w	d5,d0	; accelerate right in the air
		cmp.w	d6,d0	; compare new speed to top speed
		blt.s	+	; if new speed is less than maximum,branch
		move.w	d6,d0	; limit speed in air going right,even if Sonic was already going faster (speed limit/cap)
; Obj01_JumpMove:
+		move.w	d0,$10(a0)

; loc_1034A: Obj01_ResetScr2:
Obj01_Jump_ResetScr:
		cmpi.w	#$60,(Camera_Y_pos_bias).w	; is screen in its default position?
		beq.s	Sonic_JumpPeakDecelerate	; if yes,branch
		bhs.s	+			; depending on the sign of the difference,
		addq.w	#4,(Camera_Y_pos_bias).w	; either add 2
+		subq.w	#2,(Camera_Y_pos_bias).w	; or subtract 2

; loc_1035C:
Sonic_JumpPeakDecelerate:
		cmpi.w	#-$400,$12(a0)	; is Sonic moving faster than -$400 upwards?
		blo.s	return_1038A	; if yes,branch
		move.w	$10(a0),d0
		move.w	d0,d1
		asr.w	#5,d1		; d1 = x_velocity / 32
		beq.s	return_1038A	; return of d1 is 0
		bmi.s	Sonic_JumpPeakDecelerateLeft	; branch if moving left

; Sonic_JumpPeakDecelerateRight:
		sub.w	d1,d0	; reduce x velocity by d1
		bhs.s	+
		move.w	#0,d0
+
		move.w	d0,$10(a0)
		rts
;-------------------------------------------------------------
; loc_1037E:
Sonic_JumpPeakDecelerateLeft:
		sub.w	d1,d0	; reduce x velocity by d1
		blo.s	+
		move.w	#0,d0
+
		move.w	d0,$10(a0)

return_1038A:
		rts
; End of subroutine Sonic_ChgJumpDir

; ---------------------------------------------------------------------------
; Subroutine to prevent Sonic from leaving the boundaries of a level
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_1038C: Sonic_LevelBoundaries:
Sonic_LevelBound:
		move.l	x_pos(a0),d1
		move.w	$10(a0),d0
		ext.l	d0
		asl.l	#8,d0
		add.l	d0,d1
		swap	d1
		move.w	(Camera_Min_X_pos).w,d0
		addi.w	#$10,d0
		cmp.w	d1,d0		; has Sonic touched the left boundary?
		bhi.s	Sonic_Boundary_Sides	; if yes,branch
		move.w	(Camera_Max_X_pos).w,d0
		addi.w	#320-24,d0	; screen width - Sonic's width_pixels
		tst.b	(Current_Boss_ID).w
		bne.s	loc_103BA
		addi.w	#$40,d0

loc_103BA:
		cmp.w	d1,d0		; has Sonic touched the right boundary?
		bls.s	Sonic_Boundary_Sides	; if yes,branch
; loc_103BE:
Sonic_Boundary_CheckBottom:
		move.w	(Camera_Max_Y_pos_now).w,d0
		addi.w	#$E0,d0
		cmp.w	y_pos(a0),d0
		blt.s	Sonic_Boundary_Bottom
		rts
; ===========================================================================
; loc_103CE:
Sonic_Boundary_Bottom:

	if RemoveJmpTos
JmpTo_KillCharacter
	endif
		jmpto	JmpTo_KillCharacter
; ---------------------------------------------------------------------------
; Leftover from Sonic 1,which would transport the player to SBZ3/LZ4 upon
; reaching a certain position; its ID is different,for whatever reason
		cmpi.w	#death_egg_zone_act_2,(Current_ZoneAndAct).w	; is it DEZ2?
		bne.w	JmpTo_KillCharacter			; if not,branch
		cmpi.w	#$2000,(MainCharacter+8).w		; is Sonic beyond x position $2000?
		blo.w	JmpTo_KillCharacter			; if not,branch
		clr.b	(Last_star_pole_hit).w
		move.w	#1,(Level_Inactive_flag).w
		move.w	#labyrinth_zone_act_4,(Current_ZoneAndAct).w	; restart in OWZ4
		rts
; ===========================================================================
; loc_103F8:
Sonic_Boundary_Sides:
		move.w	d0,x_pos(a0)
		move.w	#0,$A(a0)
		move.w	#0,$10(a0)
		move.w	#0,$14(a0)
		bra.s	Sonic_Boundary_CheckBottom
; End of function Sonic_LevelBound

; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine allowing Sonic to start rolling when he's moving
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10410:
Sonic_Roll:
		tst.b	(Sliding_flag).w
		bne.s	Obj01_NoRoll
		move.w	$14(a0),d0
		bpl.s	loc_1041E
		neg.w	d0

loc_1041E:
		cmpi.w	#$80,d0		; is Sonic moving at $80 speed or faster?
		blo.s	Obj01_NoRoll	; if not,branch
		move.b	(Ctrl_1_Held_Logical).w,d0
		andi.b	#button_left_mask+button_right_mask,d0		; is left/right being pressed?
		bne.s	Obj01_NoRoll	; if yes,branch
		btst	#button_down,(Ctrl_1_Held_Logical).w	; is down being pressed?
		bne.s	Obj01_ChkRoll	; if yes,branch
; return_10436: Sonic_NoRoll:
Obj01_NoRoll:
		rts
; ---------------------------------------------------------------------------
; loc_10438:
Obj01_ChkRoll:
		btst	#2,$22(a0)
		beq.s	Obj01_DoRoll
		rts
; ---------------------------------------------------------------------------
; loc_10442: Sonic_DoRoll:
Obj01_DoRoll:
		bset	#2,$22(a0)
		move.b	#$E,$16(a0)
		move.b	#7,$17(a0)
		move.b	#2,$1C(a0)	; use "rolling" animation
		addq.w	#5,y_pos(a0)
		move.w	#SndID_Roll,d0
		jsr	(PlaySound).l	; play rolling sound
		tst.w	$14(a0)
		bne.s	return_10474
		move.w	#$200,$14(a0)

return_10474:
		rts
; End of function Sonic_Roll

; ---------------------------------------------------------------------------
; Subroutine allowing Sonic to jump
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10476:
Sonic_Jump:
		move.b	(Ctrl_1_Press_Logical).w,d0
		andi.b	#button_A_mask+button_B_mask+button_C_mask,d0		; is A,B or C pressed?
		beq.w	return_1051A	; if not,branch
		moveq	#0,d0
		move.b	$26(a0),d0
		addi.b	#$80,d0
		bsr.w	loc_136F2
		cmpi.w	#6,d1		; does Sonic have enough room to jump?
		blt.w	return_1051A	; if not,branch
		move.w	#$680,d2
		btst	#6,$22(a0)	; is Sonic underwater?
		beq.s	+		; if not,branch
		move.w	#$380,d2	; reduce jump speed
+
		moveq	#0,d0
		move.b	$26(a0),d0
		subi.b	#$40,d0
		jsr	(CalcSine).l
		muls.w	d2,d1
		asr.l	#8,d1
		add.w	d1,$10(a0)	; make Sonic jump (in X... this adds nothing on level ground)
		muls.w	d2,d0
		asr.l	#8,d0
		add.w	d0,$12(a0)	; make Sonic jump (in Y)
		bset	#1,$22(a0)
		bclr	#5,$22(a0)
		addq.l	#4,sp
		move.b	#1,$3C(a0)
		clr.b	$38(a0)
		move.w	#SndID_Jump,d0
		jsr	(PlaySound).l	; play jumping sound
		move.b	#$13,$16(a0)
		move.b	#9,$17(a0)
		btst	#2,$22(a0)
		bne.s	Sonic_RollJump
		move.b	#$E,$16(a0)
		move.b	#7,$17(a0)
		move.b	#2,$1C(a0)	; use "jumping" animation
		bset	#2,$22(a0)
		addq.w	#5,y_pos(a0)

return_1051A:
		rts
; ---------------------------------------------------------------------------
; loc_1051C:
Sonic_RollJump:
		bset	#4,$22(a0)	; set the rolling+jumping flag
		rts
; End of function Sonic_Jump

; ---------------------------------------------------------------------------
; Subroutine letting Sonic control the height of the jump
; when the jump button is released
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10524:
Sonic_JumpHeight:
		tst.b	$3C(a0)		; is Sonic jumping?
		beq.s	Sonic_UpVelCap	; if not,branch

		move.w	#-$400,d1
		btst	#6,$22(a0)	; is Sonic underwater?
		beq.s	loc_1053A	; if not,branch
		move.w	#-$200,d1

loc_1053A:
		cmp.w	$12(a0),d1	; is Sonic going up faster than d1?
		ble.s	return_1054E	; if not,branch
		move.b	(Ctrl_1_Held_Logical).w,d0
		andi.b	#button_A_mask+button_B_mask+button_C_mask,d0		; is A/B/C pressed?
		bne.s	return_1054E	; if yes,branch
		move.w	d1,$12(a0)	; immediately reduce Sonic's upward speed to d1

return_1054E:
		rts
; ---------------------------------------------------------------------------
; loc_10550:
Sonic_UpVelCap:
		cmpi.w	#-$FC0,$12(a0)	; is Sonic moving up really fast?
		bge.s	return_1055E	; if not,branch
		move.w	#-$FC0,$12(a0)	; cap upward speed

return_1055E:
		rts
; End of subroutine Sonic_JumpHeight

; ---------------------------------------------------------------------------
; Subroutine to check for starting to charge a spindash
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10560: Sonic_Spindash:
Sonic_CheckSpindash:
		tst.b	$39(a0)
		bne.s	Sonic_UpdateSpindash
		cmpi.b	#8,$1C(a0)
		bne.s	return_10592
		move.b	(Ctrl_1_Press_Logical).w,d0
		andi.b	#button_A_mask+button_B_mask+button_C_mask,d0
		beq.w	return_10592
		move.b	#9,$1C(a0)
		move.w	#SndID_Roll,d0
		jsr	(PlaySound).l
		addq.l	#4,sp
		move.b	#1,$39(a0)

return_10592:
		rts
; ===========================================================================
; loc_10594:
Sonic_UpdateSpindash:
		move.b	(Ctrl_1_Held_Logical).w,d0
		btst	#button_down,d0
		bne.s	Sonic_ChargingSpindash

		move.b	#$E,$16(a0)
		move.b	#7,$17(a0)
		move.b	#2,$1C(a0)
		addq.w	#5,y_pos(a0)
		move.b	#0,$39(a0)
		move.w	#$2000,(Horiz_scroll_delay_val).w
		move.w	#$800,$14(a0)
		btst	#0,$22(a0)
		beq.s	loc_105d2
		neg.w	$14(a0)

loc_105d2:
		bset	#2,$22(a0)
		rts
; ===========================================================================
; loc_105DA:
Sonic_ChargingSpindash:
		move.b	(Ctrl_1_Press_Logical).w,d0
		andi.b	#button_A_mask+button_B_mask+button_C_mask,d0
		beq.w	loc_105E8
		nop

loc_105E8:
		addq.l	#4,sp
		rts
; End of function Sonic_CheckSpindash

; ---------------------------------------------------------------------------
; Subroutine to slow Sonic walking up a slope
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_105EC:
Sonic_SlopeResist:
		move.b	$26(a0),d0
		addi.b	#$60,d0
		cmpi.b	#$C0,d0
		bhs.s	return_10620
		move.b	$26(a0),d0
		jsr	(CalcSine).l
		muls.w	#$20,d0
		asr.l	#8,d0
		tst.w	$14(a0)
		beq.s	return_10620
		bmi.s	loc_1061C
		tst.w	d0
		beq.s	return_1061A
		add.w	d0,$14(a0)

return_1061A:
		rts
; ---------------------------------------------------------------------------

loc_1061C:
		add.w	d0,$14(a0)

return_10620:
		rts
; End of subroutine Sonic_SlopeResist

; ---------------------------------------------------------------------------
; Subroutine to push Sonic down a slope while he's rolling
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10622:
Sonic_RollRepel:
		move.b	$26(a0),d0
		addi.b	#$60,d0
		cmpi.b	#$C0,d0
		bhs.s	return_1065C
		move.b	$26(a0),d0
		jsr	(CalcSine).l
		muls.w	#$50,d0
		asr.l	#8,d0
		tst.w	$14(a0)
		bmi.s	loc_10652
		tst.w	d0
		bpl.s	loc_1064C
		asr.l	#2,d0

loc_1064C:
		add.w	d0,$14(a0)
		rts
; ===========================================================================

loc_10652:
		tst.w	d0
		bmi.s	loc_10658
		asr.l	#2,d0

loc_10658:
		add.w	d0,$14(a0)

return_1065C:
		rts
; End of function Sonic_RollRepel

; ---------------------------------------------------------------------------
; Subroutine to push Sonic down a slope
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_1065E:
Sonic_SlopeRepel:
		nop
		tst.b	$38(a0)
		bne.s	return_10698
		tst.w	$2E(a0)
		bne.s	loc_1069A
		move.b	$26(a0),d0
		addi.b	#$20,d0
		andi.b	#$C0,d0
		beq.s	return_10698
		move.w	$14(a0),d0
		bpl.s	+
		neg.w	d0
+
		cmpi.w	#$280,d0
		bhs.s	return_10698
		clr.w	$14(a0)
		bset	#1,$22(a0)
		move.w	#$1E,$2E(a0)

return_10698:
		rts
; ===========================================================================

loc_1069A:
		subq.w	#1,$2E(a0)
		rts
; End of function Sonic_SlopeRepel

; ---------------------------------------------------------------------------
; Subroutine to return Sonic's angle to 0 as he jumps
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_106A0:
Sonic_JumpAngle:
		move.b	$26(a0),d0	; get Sonic's angle
		beq.s	Sonic_JumpFlip	; if already 0,branch
		bpl.s	loc_106B0	; if higher than 0,branch

		addq.b	#2,d0		; increase angle
		bhs.s	BranchTo_Sonic_JumpAngleSet
		moveq	#0,d0
; loc_106AE:
BranchTo_Sonic_JumpAngleSet:
		bra.s	Sonic_JumpAngleSet
; ===========================================================================

loc_106B0:
		subq.b	#2,d0		; decrease angle
		bhs.s	Sonic_JumpAngleSet
		moveq	#0,d0
; loc_106B6:
Sonic_JumpAngleSet:
		move.b	d0,$26(a0)
; End of function Sonic_JumpAngle
	; continue straight to Sonic_JumpFlip

; ---------------------------------------------------------------------------
; Updates Sonic's secondary angle if he's tumbling
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_106BA:
Sonic_JumpFlip:
		move.b	$27(a0),d0
		beq.s	return_106FE
		tst.w	$14(a0)
		bmi.s	Sonic_JumpLeftFlip
; loc_106C6:
Sonic_JumpRightFlip:
		move.b	$2D(a0),d1
		add.b	d1,d0
		bhs.s	BranchTo_Sonic_JumpFlipSet
		subq.b	#1,$2C(a0)
		bhs.s	BranchTo_Sonic_JumpFlipSet
		move.b	#0,$2C(a0)
		moveq	#0,d0
; loc_106DC:
BranchTo_Sonic_JumpFlipSet:
		bra.s	Sonic_JumpFlipSet
; ===========================================================================
; loc_106DE:
Sonic_JumpLeftFlip:
		tst.b	$29(a0)
		bne.s	Sonic_JumpRightFlip
		move.b	$2D(a0),d1
		sub.b	d1,d0
		bhs.s	Sonic_JumpFlipSet
		subq.b	#1,$2C(a0)
		bhs.s	Sonic_JumpFlipSet
		move.b	#0,$2C(a0)
		moveq	#0,d0
; loc_106FA:
Sonic_JumpFlipSet:
		move.b	d0,$27(a0)

return_106FE:
		rts
; End of function Sonic_JumpAngle

; ---------------------------------------------------------------------------
; Subroutine for Sonic to interact with the floor and walls when he's in the air
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10700: Sonic_Floor:
Sonic_DoLevelCollision:
		move.l	#Primary_Collision,(Collision_addr).w
		cmpi.b	#$C,$3E(a0)
		beq.s	loc_10718
		move.l	#Secondary_Collision,(Collision_addr).w

loc_10718:
		move.b	$3F(a0),d5
		move.w	$10(a0),d1
		move.w	$12(a0),d2
		jsr	(CalcAngle).l
		move.b	d0,$2B(a0)
		subi.b	#$20,d0
		andi.b	#$C0,d0
		cmpi.b	#$40,d0
		beq.w	Sonic_HitLeftWall
		cmpi.b	#$80,d0
		beq.w	Sonic_HitCeilingAndWalls
		cmpi.b	#$C0,d0
		beq.w	Sonic_HitRightWall
		bsr.w	Sonic_HitWall
		tst.w	d1
		bpl.s	loc_10760
		sub.w	d1,x_pos(a0)
		move.w	#0,$10(a0)

loc_10760:
		bsr.w	loc_1397A
		tst.w	d1
		bpl.s	loc_10772
		add.w	d1,x_pos(a0)
		move.w	#0,$10(a0)

loc_10772:
		bsr.w	loc_13736
		tst.w	d1
		bpl.s	return_107EA
		move.b	$12(a0),d2
		addq.b	#8,d2
		neg.b	d2
		cmp.b	d2,d1
		bge.s	loc_1078A
		cmp.b	d2,d0
		blt.s	return_107EA

loc_1078A:
		add.w	d1,y_pos(a0)
		move.b	d3,$26(a0)
		bsr.w	Sonic_ResetOnFloor
		move.b	#0,$1C(a0)
		move.b	d3,d0
		addi.b	#$20,d0
		andi.b	#$40,d0
		bne.s	loc_107C8
		move.b	d3,d0
		addi.b	#$10,d0
		andi.b	#$20,d0
		beq.s	loc_107BA
		asr.w	$12(a0)
		bra.s	loc_107DC
; ===========================================================================

loc_107BA:
		move.w	#0,$12(a0)
		move.w	$10(a0),$14(a0)
		rts
; ===========================================================================

loc_107C8:
		move.w	#0,$10(a0)	; stop Sonic since he hit a wall
		cmpi.w	#$FC0,$12(a0)
		ble.s	loc_107DC
		move.w	#$FC0,$12(a0)

loc_107DC:
		move.w	$12(a0),$14(a0)
		tst.b	d3
		bpl.s	return_107EA
		neg.w	$14(a0)

return_107EA:
		rts
; ===========================================================================
; loc_107EC:
Sonic_HitLeftWall:
		bsr.w	Sonic_HitWall
		tst.w	d1
		bpl.s	Sonic_HitCeiling
		sub.w	d1,x_pos(a0)
		move.w	#0,$10(a0)	; stop Sonic since he hit a wall
		move.w	$12(a0),$14(a0)
		rts
; ===========================================================================
; loc_10806:
Sonic_HitCeiling:
		bsr.w	Sonic_DontRunOnWalls
		tst.w	d1
		bpl.s	Sonic_HitFloor
		sub.w	d1,y_pos(a0)
		tst.w	$12(a0)
		bpl.s	return_1081E
		move.w	#0,$12(a0)	; stop Sonic since he hit a ceiling

return_1081E:
		rts
; ===========================================================================
; loc_10820:
Sonic_HitFloor:
		tst.w	$12(a0)
		bmi.s	return_1084C
		bsr.w	loc_13736
		tst.w	d1
		bpl.s	return_1084C
		add.w	d1,y_pos(a0)
		move.b	d3,$26(a0)
		bsr.w	Sonic_ResetOnFloor
		move.b	#0,$1C(a0)
		move.w	#0,$12(a0)
		move.w	$10(a0),$14(a0)

return_1084C:
		rts
; ===========================================================================
; loc_1084E:
Sonic_HitCeilingAndWalls:
		bsr.w	Sonic_HitWall
		tst.w	d1
		bpl.s	+
		sub.w	d1,x_pos(a0)
		move.w	#0,$10(a0)	; stop Sonic since he hit a wall
+
		bsr.w	loc_1397A
		tst.w	d1
		bpl.s	+
		add.w	d1,x_pos(a0)
		move.w	#0,$10(a0)	; stop Sonic since he hit a wall
+
		bsr.w	Sonic_DontRunOnWalls
		tst.w	d1
		bpl.s	return_108A8
		sub.w	d1,y_pos(a0)
		move.b	d3,d0
		addi.b	#$20,d0
		andi.b	#$40,d0
		bne.s	loc_10892
		move.w	#0,$12(a0)	; stop Sonic since he hit a ceiling
		rts
; ===========================================================================

loc_10892:
		move.b	d3,$26(a0)
		bsr.w	Sonic_ResetOnFloor
		move.w	$12(a0),$14(a0)
		tst.b	d3
		bpl.s	return_108A8
		neg.w	$14(a0)

return_108A8:
		rts
; ===========================================================================
; loc_108AA:
Sonic_HitRightWall:
		bsr.w	loc_1397A
		tst.w	d1
		bpl.s	Sonic_HitCeiling2
		add.w	d1,x_pos(a0)
		move.w	#0,$10(a0)	; stop Sonic since he hit a wall
		move.w	$12(a0),$14(a0)
		rts
; ===========================================================================
; identical to Sonic_HitCeiling...
; loc_108C4:
Sonic_HitCeiling2:
		bsr.w	Sonic_DontRunOnWalls
		tst.w	d1
		bpl.s	Sonic_HitFloor2
		sub.w	d1,y_pos(a0)
		tst.w	$12(a0)
		bpl.s	return_108DC
		move.w	#0,$12(a0)	; stop Sonic since he hit a ceiling

return_108DC:
		rts
; ===========================================================================
; identical to Sonic_HitFloor...
loc_108DE:
Sonic_HitFloor2:
		tst.w	$12(a0)
		bmi.s	return_1090A
		bsr.w	loc_13736
		tst.w	d1
		bpl.s	return_1090A
		add.w	d1,y_pos(a0)
		move.b	d3,$26(a0)
		bsr.w	Sonic_ResetOnFloor
		move.b	#0,$1C(a0)
		move.w	#0,$12(a0)	; stop Sonic since he hit a ceiling
		move.w	$10(a0),$14(a0)

return_1090A:
		rts
; End of function Sonic_DoLevelCollision

; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to reset Sonic's mode when he lands on the floor
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_1090C:
Sonic_ResetOnFloor:
		btst	#4,$22(a0)
		beq.s	loc_1091A
		nop
		nop
		nop

loc_1091A:
		bclr	#5,$22(a0)
		bclr	#1,$22(a0)
		bclr	#4,$22(a0)
		btst	#2,$22(a0)
		beq.s	loc_10950
		bclr	#2,$22(a0)
		move.b	#$13,$16(a0)
		move.b	#9,$17(a0)
		move.b	#0,$1C(a0)
		subq.w	#5,y_pos(a0)

loc_10950:
		move.b	#0,$3C(a0)
		move.w	#0,(Chain_Bonus_counter).w
		move.b	#0,$27(a0)
		move.b	#0,$29(a0)
		rts
; End of function Sonic_ResetOnFloor

; ===========================================================================
; ---------------------------------------------------------------------------
; Sonic when he gets hurt
; ---------------------------------------------------------------------------
; loc_1096A: Sonic_Hurt:
Obj01_Hurt:
		tst.b	$25(a0)
		bmi.w	Sonic_HurtInstantRecover
		jsr	(ObjectMove).l
		addi.w	#$30,$12(a0)
		btst	#6,$22(a0)
		beq.s	loc_1098C
		subi.w	#$20,$12(a0)

loc_1098C:
		bsr.w	Sonic_HurtStop
		bsr.w	Sonic_LevelBound
		bsr.w	Sonic_RecordPos
		bsr.w	Sonic_Animate
		bsr.w	LoadSonicDynPLC
		jmp	(DisplaySprite).l
; ===========================================================================
; loc_109A6:
Sonic_HurtStop:
		move.w	(Camera_Max_Y_pos_now).w,d0
		addi.w	#$E0,d0
		cmp.w	y_pos(a0),d0
		blo.w	JmpTo_KillCharacter
		bsr.w	Sonic_DoLevelCollision
		btst	#1,$22(a0)
		bne.s	+	; rts
		moveq	#0,d0
		move.w	d0,$12(a0)
		move.w	d0,$10(a0)
		move.w	d0,$14(a0)
		move.b	#0,$1C(a0)
		subq.b	#2,routine(a0)
		move.w	#120,$30(a0)
+
		rts
; End of function Obj01_Hurt

; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to make Sonic recover control after getting hit but before landing
; ---------------------------------------------------------------------------
; loc_109E2:
Sonic_HurtInstantRecover:
		subq.b	#2,routine(a0)
		move.b	#0,$25(a0)
		bsr.w	Sonic_RecordPos
		bsr.w	Sonic_Animate
		bsr.w	LoadSonicDynPLC
		jmp	(DisplaySprite).l
; End of function Sonic_HurtInstantRecover

; ===========================================================================
; ---------------------------------------------------------------------------
; Sonic when he dies
; ---------------------------------------------------------------------------
; loc_109FE: Sonic_Death:
Obj01_Dead:
		bsr.w	CheckGameOver
		jsr	(ObjectMoveAndFall).l
		bsr.w	Sonic_RecordPos
		bsr.w	Sonic_Animate
		bsr.w	LoadSonicDynPLC
		jmp	(DisplaySprite).l

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10A1A: Sonic_GameOver:
CheckGameOver:
		move.w	(Camera_Max_Y_pos_now).w,d0
		addi.w	#$100,d0
		cmp.w	y_pos(a0),d0
		bhs.w	return_10A9C
		move.w	#-$38,$12(a0)
		addq.b	#2,routine(a0)
		clr.b	(Update_HUD_timer).w
		addq.b	#1,(Update_HUD_lives).w
		subq.b	#1,(Life_count).w
		bne.s	Obj01_ResetLevel
		move.w	#0,$3A(a0)
		move.b	#$39,(GameOver_GameText).w
		move.b	#$39,(GameOver_OverText).w
		move.b	#1,(GameOver_OverText+$1A).w
		clr.b	(Time_Over_flag).w
; loc_10A5E:
Obj01_Finished:
		move.w	#MusID_GameOver,d0
		jsr	(PlayMusic).l
		moveq	#PLCID_GameOver,d0
		jmp	(LoadPLC).l
; End of function CheckGameOver

; ===========================================================================
; ---------------------------------------------------------------------------
; Sonic when the level is restarted
; ---------------------------------------------------------------------------
; loc_10A70:
Obj01_ResetLevel:
		move.w	#60,$3A(a0)
		tst.b	(Time_Over_flag).w
		beq.s	return_10A9C
		move.w	#0,$3A(a0)
		move.b	#$39,(TimeOver_TimeText).w
		move.b	#$39,(TimeOver_OverText).w
		move.b	#2,(TimeOver_TimeText+$1A).w
		move.b	#3,(TimeOver_OverText+$1A).w
		bra.s	Obj01_Finished

return_10A9C:
		rts
; End of function Obj01_ResetLevel

; ===========================================================================
; ---------------------------------------------------------------------------
; Sonic when he's offscreen and waiting for the level to restart
; ---------------------------------------------------------------------------
; loc_10A9E: Sonic_ResetLevel:
Obj01_Gone:
		tst.w	$3A(a0)
		beq.s	+
		subq.w	#1,$3A(a0)
		bne.s	+
		move.w	#1,(Level_Inactive_flag).w
+
		rts
; End of function Obj01_Gone

; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to animate Sonic's sprites
; See also: AnimateSprite
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10AB2:
Sonic_Animate:
		lea	(SonicAniData).l,a1
		moveq	#0,d0
		move.b	$1C(a0),d0
		cmp.b	$1D(a0),d0	; has the animation changed?
		beq.s	SAnim_Do	; if not,branch
		move.b	d0,$1D(a0)	; set previous animation
		move.b	#0,$1B(a0)	; reset animation frame
		move.b	#0,$1E(a0)	; reset animation duration
		bclr	#5,$22(a0)	; clear 'pushing' flag
; loc_10ADA:
SAnim_Do:
		add.w	d0,d0
		adda.w	(a1,d0.w),a1	; calculate address of appropriate animation script
		move.b	(a1),d0
		bmi.s	SAnim_WalkRun	; if animation is walk/run/roll/jump,branch
		move.b	$22(a0),d1
		andi.b	#1,d1
		andi.b	#$FC,1(a0)
		or.b	d1,1(a0)
		subq.b	#1,$1E(a0)	; subtract 1 from frame duration
		bpl.s	SAnim_Delay	; if time remains,branch
		move.b	d0,$1E(a0)	; load frame duration
; loc_10B00:
SAnim_Do2:
		moveq	#0,d1
		move.b	$1B(a0),d1	; load current frame number
		move.b	1(a1,d1.w),d0	; read sprite number from script
		cmpi.b	#$F0,d0
		bhs.s	SAnim_End_FF	; if animation is complete,branch
; loc_10B10:
SAnim_Next:
		move.b	d0,$1A(a0)	; load sprite number
		addq.b	#1,$1B(a0)	; go to next frame
; return_10B18:
SAnim_Delay:
		rts
; ===========================================================================
; loc_10B1A:
SAnim_End_FF:
		addq.b	#1,d0		; is the end flag = $FF?
		bne.s	SAnim_End_FE	; if not,branch
		move.b	#0,$1B(a0)	; restart the animation
		move.b	1(a1),d0	; read sprite number
		bra.s	SAnim_Next
; ===========================================================================
; loc_10B2A:
SAnim_End_FE:
		addq.b	#1,d0		; is the end flag = $FE?
		bne.s	SAnim_End_FD	; if not,branch
		move.b	2(a1,d1.w),d0	; read the next byte in the script
		sub.b	d0,$1B(a0)	; jump back d0 bytes in the script
		sub.b	d0,d1
		move.b	1(a1,d1.w),d0	; read sprite number
		bra.s	SAnim_Next
; ===========================================================================
; loc_10B3E:
SAnim_End_FD:
		addq.b	#1,d0		; is the end flag = $FD?
		bne.s	SAnim_End	; if not,branch
		move.b	2(a1,d1.w),$1C(a0)	; read next byte,run that animation
; return_10B48:
SAnim_End:
		rts
; ===========================================================================
; loc_10B4A:
SAnim_WalkRun:
		subq.b	#1,$1E(a0)	; is the start flag = $FF?
		bpl.s	SAnim_Delay	; if not,branch
		addq.b	#1,d0		; is animation walking/running?
		bne.w	SAnim_Roll	; if not,branch
		moveq	#0,d0
		move.b	$27(a0),d0
		bne.w	SAnim_Tumble
		moveq	#0,d1
		move.b	$26(a0),d0	; get Sonic's angle
		move.b	$22(a0),d2
		andi.b	#1,d2		; is Sonic mirrored horizontally?
		bne.s	+		; if yes,branch
		not.b	d0		; reverse angle
+
		addi.b	#$10,d0		; add $10 to angle
		bpl.s	+		; if angle is 0-$7F,branch
		moveq	#3,d1
+
		andi.b	#$FC,1(a0)
		eor.b	d1,d2
		or.b	d2,1(a0)
		btst	#5,$22(a0)
		bne.w	SAnim_Push
		lsr.b	#4,d0		; divide angle by 16
		andi.b	#6,d0		; angle must be 0,2,4 or 6
		move.w	$14(a0),d2	; get Sonic's "speed" for animation purposes
		bpl.s	+
		neg.w	d2
+
		lea	(SonAni_Run).l,a1	; use running animation
		cmpi.w	#$600,d2		; is Sonic at running speed?
		bhs.s	+			; if yes,branch
		lea	(SonAni_Walk).l,a1	; use walking animation
+
		move.b	d0,d1
		lsr.b	#1,d1
		add.b	d1,d0
		add.b	d0,d0
		add.b	d0,d0
		move.b	d0,d3
		neg.w	d2
		addi.w	#$800,d2
		bpl.s	+
		moveq	#0,d2
+
		lsr.w	#8,d2
		lsr.w	#1,d2
		move.b	d2,$1E(a0)	; modify frame duration
		bsr.w	SAnim_Do2
		add.b	d3,$1A(a0)	; modify frame number
		rts
; ===========================================================================
; loc_10BD8:
SAnim_Tumble:
		move.b	$27(a0),d0
		moveq	#0,d1
		move.b	$22(a0),d2
		andi.b	#1,d2
		bne.s	SAnim_Tumble_Left

		andi.b	#$FC,1(a0)
		addi.b	#$B,d0
		divu.w	#$16,d0
		addi.b	#$9B,d0
		move.b	d0,$1A(a0)
		move.b	#0,$1E(a0)
		rts
; ===========================================================================
; loc_10C06:
SAnim_Tumble_Left:
		andi.b	#$FC,1(a0)
		tst.b	$29(a0)
		beq.s	loc_10C1E
		ori.b	#1,1(a0)
		addi.b	#$B,d0
		bra.s	loc_10C2A
; ===========================================================================

loc_10C1E:
		ori.b	#3,1(a0)
		neg.b	d0
		addi.b	#$8F,d0

loc_10C2A:
		divu.w	#$16,d0
		addi.b	#$9B,d0
		move.b	d0,$1A(a0)
		move.b	#0,$1E(a0)
		rts
; ===========================================================================
; loc_10C3E:
SAnim_Roll:
		addq.b	#1,d0		; is the start flag = $FE?
		bne.s	SAnim_Push	; if not,branch
		move.w	$14(a0),d2
		bpl.s	+
		neg.w	d2
+
		lea	(SonAni_Roll2).l,a1
		cmpi.w	#$600,d2
		bhs.s	+
		lea	(SonAni_Roll).l,a1
+
		neg.w	d2
		addi.w	#$400,d2
		bpl.s	+
		moveq	#0,d2
+
		lsr.w	#8,d2
		move.b	d2,$1E(a0)
		move.b	$22(a0),d1
		andi.b	#1,d1
		andi.b	#$FC,1(a0)
		or.b	d1,1(a0)
		bra.w	SAnim_Do2
; ===========================================================================
; loc_10C82:
SAnim_Push:
		move.w	$14(a0),d2
		bmi.s	+
		neg.w	d2
+
		addi.w	#$800,d2
		bpl.s	+
		moveq	#0,d2
+
		lsr.w	#6,d2
		move.b	d2,$1E(a0)
		lea	(SonAni_Push).l,a1
		move.b	$22(a0),d1
		andi.b	#1,d1
		andi.b	#$FC,1(a0)
		or.b	d1,1(a0)
		bra.w	SAnim_Do2
; ===========================================================================
; ---------------------------------------------------------------------------
; Animation script - Sonic
; ---------------------------------------------------------------------------
; off_10CB4: Sonic_AnimateData:
SonicAniData:	offsetTable
		offsetTableEntry.w SonAni_Walk
		offsetTableEntry.w SonAni_Run
		offsetTableEntry.w SonAni_Roll
		offsetTableEntry.w SonAni_Roll2
		offsetTableEntry.w SonAni_Push
		offsetTableEntry.w SonAni_Wait
		offsetTableEntry.w SonAni_Balance
		offsetTableEntry.w SonAni_LookUp
		offsetTableEntry.w SonAni_Duck
		offsetTableEntry.w SonAni_Spindash
		offsetTableEntry.w SonAni_WallRecoil1
		offsetTableEntry.w SonAni_WallRecoil2
		offsetTableEntry.w SonAni_0x0C
		offsetTableEntry.w SonAni_Stop
		offsetTableEntry.w SonAni_Float1
		offsetTableEntry.w SonAni_Float2
		offsetTableEntry.w SonAni_0x10
		offsetTableEntry.w SonAni_S1LzHang
		offsetTableEntry.w SonAni_Unused_0x12
		offsetTableEntry.w SonAni_Unused_0x13
		offsetTableEntry.w SonAni_Unused_0x14
		offsetTableEntry.w SonAni_Bubble
		offsetTableEntry.w SonAni_Death1
		offsetTableEntry.w SonAni_Drown
		offsetTableEntry.w SonAni_Death2
		offsetTableEntry.w SonAni_Unused_0x19
		offsetTableEntry.w SonAni_Hurt
		offsetTableEntry.w SonAni_S1LzSlide
		offsetTableEntry.w SonAni_0x1C
		offsetTableEntry.w SonAni_Float3
		offsetTableEntry.w SonAni_0x1E
SonAni_Walk:		dc.b $FF,$10,$11,$12,$13,$14,$15,$16,$17,$0C,$0D,$0E,$0F,$FF
SonAni_Run:		dc.b $FF,$3C,$3D,$3E,$3F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
SonAni_Roll:		dc.b $FE,$6C,$70,$6D,$70,$6E,$70,$6F,$70,$FF
SonAni_Roll2:		dc.b $FE,$6C,$70,$6D,$70,$6E,$70,$6F,$70,$FF
SonAni_Push:		dc.b $FD,$77,$78,$79,$7A,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
SonAni_Wait:		dc.b $07,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
			dc.b $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02
			dc.b $03,$03,$03,$04,$04,$05,$05,$FE,$04
SonAni_Balance:		dc.b $07,$89,$8A,$FF
SonAni_LookUp:		dc.b $05,$06,$07,$FE,$01
SonAni_Duck:		dc.b $05,$7F,$80,$FE,$01
SonAni_Spindash:	dc.b $00,$71,$72,$71,$73,$71,$74,$71,$75,$71,$76,$71,$FF
SonAni_WallRecoil1:	dc.b $3F,$82,$FF
SonAni_WallRecoil2:	dc.b $07,$08,$08,$09,$FD,$05
SonAni_0x0C:		dc.b $07,$09,$FD,$05
SonAni_Stop:		dc.b $03,$81,$82,$83,$84,$85,$86,$87,$88,$FE,$02
SonAni_Float1:		dc.b $07,$94,$96,$FF
SonAni_Float2:		dc.b $07,$91,$92,$93,$94,$95,$FF
SonAni_0x10:		dc.b $2F,$7E,$FD,$00
SonAni_S1LzHang:	dc.b $05,$8F,$90,$FF
SonAni_Unused_0x12:	dc.b $0F,$43,$43,$43,$FE,$01
SonAni_Unused_0x13:	dc.b $0F,$43,$44,$FE,$01
SonAni_Unused_0x14:	dc.b $3F,$49,$FF
SonAni_Bubble:		dc.b $0B,$97,$97,$12,$13,$FD,$00
SonAni_Death1:		dc.b $20,$9A,$FF
SonAni_Drown:		dc.b $20,$99,$FF
SonAni_Death2:		dc.b $20,$98,$FF
SonAni_Unused_0x19:	dc.b $03,$4E,$4F,$50,$51,$52,$00,$FE,$01
SonAni_Hurt:		dc.b $40,$8D,$FF
SonAni_S1LzSlide:	dc.b $09,$8D,$8E,$FF
SonAni_0x1C:		dc.b $77,$00,$FD,$00
SonAni_Float3:		dc.b $03,$91,$92,$93,$94,$95,$FF
SonAni_0x1E:		dc.b $03,$3C,$FD,$00
		even

; ---------------------------------------------------------------------------
; Sonic pattern loading subroutine
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10DDC: Load_Sonic_Dynamic_PLC:
LoadSonicDynPLC:
		moveq	#0,d0
		move.b	$1A(a0),d0	; load frame number
		cmp.b	(Sonic_LastLoadedDPLC).w,d0
		beq.s	return_10E2E
		move.b	d0,(Sonic_LastLoadedDPLC).w
		lea	(MapRUnc_Sonic).l,a2
		add.w	d0,d0
		adda.w	(a2,d0.w),a2
		move.w	(a2)+,d5
		subq.w	#1,d5
		bmi.s	return_10E2E
		move.w	#$F000,d4
; loc_10E02:
SPLC_ReadEntry:
		moveq	#0,d1
		move.w	(a2)+,d1
		move.w	d1,d3
		lsr.w	#8,d3
		andi.w	#$F0,d3
		addi.w	#$10,d3
		andi.w	#$FFF,d1
		lsl.l	#5,d1
		addi.l	#ArtUnc_Sonic,d1
		move.w	d4,d2
		add.w	d3,d4
		add.w	d3,d4
		jsr	(QueueDMATransfer).l
		dbf	d5,SPLC_ReadEntry	; repeat for number of entries

return_10E2E:
		rts
; End of function LoadSonicDynPLC