; ===========================================================================
; ---------------------------------------------------------------------------
; Object 02 - Tails
; ---------------------------------------------------------------------------
; Sprite_10E38: Obj_0x02_Tails:
Obj02:
		moveq	#0,d0
		move.b	routine(a0),d0
		move.w	Obj02_Index(pc,d0.w),d1
		jmp	Obj02_Index(pc,d1.w)
; ===========================================================================
; off_10E46: Tails_Index:
Obj02_Index:	offsetTable
		offsetTableEntry.w Obj02_Init
		offsetTableEntry.w Obj02_Control
		offsetTableEntry.w Tails_Hurt
		offsetTableEntry.w Tails_Death
		offsetTableEntry.w Tails_ResetLevel
; ===========================================================================
; loc_10E50: Tails_Main:
Obj02_Init:
		addq.b	#2,routine(a0)
		move.b	#$F,$16(a0)
		move.b	#9,$17(a0)
		move.l	#Tails_Mappings,4(a0)
		move.w	#$7A0,2(a0)
		bsr.w	Adjust2PArtPointer
		move.b	#2,$18(a0)
		move.b	#$18,$19(a0)
		move.b	#$84,1(a0)
		move.w	#$600,(Sonic_top_speed).w
		move.w	#$C,(Sonic_acceleration).w
		move.w	#$80,(Sonic_deceleration).w
		move.b	#$C,$3E(a0)
		move.b	#$D,$3F(a0)
		move.b	#0,$2C(a0)
		move.b	#4,$2D(a0)
		move.b	#5,(Tails_Tails).w
; loc_10EB4: Tails_Control:
Obj02_Control:
		bsr.w	TailsCPU_Control
		btst	#0,$2A(a0)
		bne.s	Obj02_ControlsLock
		moveq	#0,d0
		move.b	$22(a0),d0
		andi.w	#6,d0
		move.w	Obj02_Modes(pc,d0.w),d1
		jsr	Obj02_Modes(pc,d1.w)
; loc_10Ed2: Tails_ControlsLock:
Obj02_ControlsLock:
		bsr.s	Tails_Display
		bsr.w	Tails_RecordPos
		move.b	(Primary_Angle).w,$36(a0)
		move.b	(Secondary_Angle).w,$37(a0)
		bsr.w	Tails_Animate
		tst.b	$2A(a0)
		bmi.s	loc_10EF4
		jsr	(TouchResponse).l

loc_10EF4:
		bsr.w	LoadTailsDynPLC
		rts
; ===========================================================================
; off_10EFA: Tails_Modes:
Obj02_Modes:	offsetTable
		offsetTableEntry.w Obj02_MdNormal	; 0 - not airborne or rolling
		offsetTableEntry.w Obj02_MdAir		; 2 - airborne
		offsetTableEntry.w Obj02_MdRoll		; 4 - rolling
		offsetTableEntry.w Obj02_MdJump		; 6 - jumping
; ===========================================================================
; byte_10F02:
Tails_MusicList:	zoneOrderedTable 1,1
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

; loc_10F12:
Tails_Display:
		move.w	$30(a0),d0
		beq.s	Obj02_Display
		subq.w	#1,$30(a0)
		lsr.w	#3,d0
		bhs.s	Obj02_ChkInvin
; loc_10F20:
Obj02_Display:
		jsr	(DisplaySprite).l
; loc_10F26:
Obj02_ChkInvin:	; Checks if Tails has run out of invincibility frames
		tst.b	(Invincibility_flag).w
		beq.s	Obj02_ChkShoes
		tst.w	$32(a0)
		beq.s	Obj02_ChkShoes
		subq.w	#1,$32(a0)
		bne.s	Obj02_ChkShoes
		tst.b	(Current_Boss_ID).w
		bne.s	Obj02_RmvInvin
		cmpi.w	#$C,(Current_Air).w
		blo.s	Obj02_RmvInvin
		moveq	#0,d0
		move.b	(Current_Zone).w,d0
		lea	Tails_MusicList(pc),a1
		move.b	(a1,d0.w),d0
		jsr	(PlayMusic).l
; loc_10F5A:
Obj02_RmvInvin:
		move.b	#0,(Invincibility_flag).w
; loc_10F60:
Obj02_ChkShoes:	; Checks if Tails should still have the speed shoes
		tst.b	(Speed_shoes).w
		beq.s	Obj02_ExitChk
		tst.w	$34(a0)
		beq.s	Obj02_ExitChk
		subq.w	#1,$34(a0)
		bne.s	Obj02_ExitChk
		move.w	#$600,(Sonic_top_speed).w
		move.w	#$C,(Sonic_acceleration).w
		move.w	#$80,(Sonic_deceleration).w
		move.b	#0,(Speed_shoes).w
		move.w	#MusID_SlowDown,d0	; restore music tempo
		jmp	(PlayMusic).l
; return_10F94:
Obj02_ExitChk:
		rts
; End of subroutine Tails_Display

; ---------------------------------------------------------------------------
; Tails' AI code; rather idiotic in this version,as it only really is
; programmed to copy Sonic's inputs and make no effort to correct itself
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_10F96: Tails_Control2:
TailsCPU_Control:
		move.b	(Ctrl_2_Held).w,d0
		andi.b	#button_up_mask+button_down_mask+button_left_mask+button_right_mask+button_A_mask+button_B_mask+button_C_mask,d0	; did the real player 2 hit something?
		beq.s	TailsCPU_Normal_HumanControl		; if not,branch
		move.w	#0,(unk_F700).w				; clear this flag that is never set...
		move.w	#60*5,(Tails_control_counter).w		; give player 2 control for 5 seconds
		rts
; ===========================================================================
; loc_10FAE: Tails_ControlNoKeysPressed:
TailsCPU_Normal_HumanControl:
		tst.w	(Tails_control_counter).w
		beq.s	+
		subq.w	#1,(Tails_control_counter).w
		rts
+
		move.w	(Tails_CPU_routine).w,d0
		move.w	TailsCPU_States(pc,d0.w),d0
		jmp	TailsCPU_States(pc,d0.w)
; ===========================================================================
; off_10FC6: Tails_ControlIndex:
TailsCPU_States: offsetTable
		offsetTableEntry.w TailsCPU_Init
		offsetTableEntry.w Tails_Control_01
		offsetTableEntry.w Tails_Control_02
		offsetTableEntry.w TailsCPU_Normal

; ===========================================================================
; initial AI State
; ---------------------------------------------------------------------------
; loc_10FCE: Tails_Control_00
TailsCPU_Init:
		move.w	#6,(Tails_CPU_routine).w
		rts
; ===========================================================================
; unused AI states,drops Tails out of the sky and... nothing else...
; for some reason they REALLY didn't want players seeing this given
; they both get stopped by branch to the used code and a return command...
; ---------------------------------------------------------------------------
; loc_10Fd6:
Tails_Control_01:
		move.w	#6,(Tails_CPU_routine).w
		rts
		move.w	#$40,(unk_F706).w
		move.w	#4,(Tails_CPU_routine).w

; loc_10FEA:
Tails_Control_02:
		move.w	#6,(Tails_CPU_routine).w
		rts
		move.w	(unk_F706).w,d1
		subq.w	#1,d1
		cmpi.w	#$10,d1
		bne.s	loc_11004
		move.w	#6,(Tails_CPU_routine).w

loc_11004:
		move.w	d1,(unk_F706).w
		lea	(unk_E600).w,a1
		lsl.b	#2,d1
		addq.b	#4,d1
		move.w	(unk_EEE0).w,d0
		sub.b	d1,d0
		move.w	(a1,d0.w),x_pos(a0)
		move.w	2(a1,d0.w),y_pos(a0)
		rts
; ===========================================================================
; AI State where Tails follows the player normally
; ---------------------------------------------------------------------------
; loc_11024: Tails_ControlCopySonicMoves:
TailsCPU_Normal:
		move.w	(MainCharacter+8).w,d0
		sub.w	x_pos(a0),d0
		bpl.s	+
		neg.w	d0
+
		cmpi.w	#224-32,d0			; is Sonic 192 pixels away from Tails?
		blo.s	TailsCPU_Normal_SonicOK		; if not,branch
		nop					; ...and then do nothing and continue anyways...
; loc_11038:
TailsCPU_Normal_SonicOK:
		; amazingly,this block of code is still in the final!
		lea	(Sonic_Pos_Record_Buf).w,a1
		move.w	#$10,d1
		lsl.b	#2,d1
		addq.b	#4,d1
		move.w	(Sonic_Pos_Record_Index).w,d0
		sub.b	d1,d0
		lea	(Sonic_Stat_Record_Buf).w,a1
		move.w	(a1,d0.w),(Ctrl_2).w
		rts

; ---------------------------------------------------------------------------
; Subroutine to record Tails' previous positions for invincibility stars
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_11056: Tails_RecordMoves:
Tails_RecordPos:
		move.w	(Tails_Pos_Record_Index).w,d0
		lea	(Tails_Pos_Record_Buf).w,a1
		lea	(a1,d0),a1
		move.w	x_pos(a0),(a1)+
		move.w	y_pos(a0),(a1)+
		addq.b	#4,(Tails_Pos_Record_Index+1).w
		rts
; End of subroutine Tails_RecordPos

; ===========================================================================
; ---------------------------------------------------------------------------
; Start of subroutine Obj02_MdNormal
; Called if Tails is neither airborne nor rolling this frame
; ---------------------------------------------------------------------------
; loc_11070: Tails_MdNormal:
Obj02_MdNormal:
		bsr.w	Tails_Spindash
		bsr.w	Tails_Jump
		bsr.w	Tails_SlopeResist
		bsr.w	Tails_Move
		bsr.w	Tails_Roll
		bsr.w	Tails_LevelBoundaries
		jsr	(ObjectMove).l
		bsr.w	AnglePos
		bsr.w	Tails_SlopeRepel
		rts
; End of subroutine Obj02_MdNormal

; ===========================================================================
; Start of subroutine Obj02_MdAir
; Called if Tails is airborne,but not in a ball (thus,probably not jumping)
; loc_11098: Tails_MdJump:
Obj02_MdAir:
		bsr.w	Tails_JumpHeight
		bsr.w	Tails_ChgJumpDir
		bsr.w	Tails_LevelBoundaries
		jsr	(ObjectMoveAndFall).l
		btst	#6,$22(a0)	; is Tails underwater?
		beq.s	+		; if not,branch
		subi.w	#$28,$12(a0)	; reduce gravity by $28 ($38-$28=$10)
+
		bsr.w	Tails_JumpAngle
		bsr.w	Tails_Floor
		rts
; End of subroutine Obj02_MdAir

; ===========================================================================
; Start of subroutine Obj02_MdRoll
; Called if Tails is in a ball,but not airborne (thus,probably rolling)
; loc_110C2: Tails_MdRoll:
Obj02_MdRoll:
		bsr.w	Tails_Jump
		bsr.w	Tails_RollRepel
		bsr.w	Tails_RollSpeed
		bsr.w	Tails_LevelBoundaries
		jsr	(ObjectMove).l
		bsr.w	AnglePos
		bsr.w	Tails_SlopeRepel
		rts
; End of subroutine Obj02_MdAir

; ===========================================================================
; Start of subroutine Obj02_MdJump
; Called if Tails is in a ball and airborne (he could be jumping but not necessarily)
; Notes: This is identical to Obj02_MdAir,at least at this outer level.
;		 Why they gave it a separate copy of the code,I don't know.
; loc_110E2: Tails_MdJump2:
Obj02_MdJump:
		bsr.w	Tails_JumpHeight
		bsr.w	Tails_ChgJumpDir
		bsr.w	Tails_LevelBoundaries
		jsr	(ObjectMoveAndFall).l
		btst	#6,$22(a0)	; is Tails underwater?
		beq.s	+		; if not,branch
		subi.w	#$28,$12(a0)	; reduce gravity by $28 ($38-$28=$10)
+
		bsr.w	Tails_JumpAngle
		bsr.w	Tails_Floor
		rts
; End of subroutine Obj02_MdJump

; ---------------------------------------------------------------------------
; Subroutine to make Tails walk/run
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_1110C:
Tails_Move:
		move.w	(Sonic_top_speed).w,d6
		move.w	(Sonic_acceleration).w,d5
		move.w	(Sonic_deceleration).w,d4
		tst.b	(Sliding_flag).w
		bne.w	Obj02_Traction
		tst.w	$2E(a0)
		bne.w	Obj02_UpdateSpeedOnGround
		btst	#button_left,(Ctrl_2_Held).w	; is left being pressed?
		beq.s	Obj02_NotLeft		; if not,branch
		bsr.w	Tails_MoveLeft
; loc_11134:
Obj02_NotLeft:
		btst	#button_right,(Ctrl_2_Held).w	; is right being pressed?
		beq.s	Obj02_NotRight		; if not,branch
		bsr.w	Tails_MoveRight
; loc_11140:
Obj02_NotRight:
		move.b	$26(a0),d0
		addi.b	#$20,d0
		andi.b	#$C0,d0				; is Tails on a slope?
		bne.w	Obj02_UpdateSpeedOnGround	; if yes,branch
		tst.w	$14(a0)				; is Tails moving?
		bne.w	Obj02_UpdateSpeedOnGround	; if yes,branch
		bclr	#5,$22(a0)
		move.b	#5,$1C(a0)		; use "standing" animation
		btst	#3,$22(a0)
		beq.s	Tails_Balance
		moveq	#0,d0
		move.b	$3D(a0),d0
		lsl.w	#6,d0
		lea	(Object_RAM).w,a1	; a1=character
		lea	(a1,d0.w),a1		; a1=object

		tst.b	$22(a1)
		bmi.s	Tails_LookUp
		moveq	#0,d1
		move.b	$19(a1),d1
		move.w	d1,d2
		add.w	d2,d2
		subq.w	#4,d2
		add.w	x_pos(a0),d1
		sub.w	x_pos(a1),d1
		cmpi.w	#4,d1
		blt.s	Tails_BalanceOnObjLeft
		cmp.w	d2,d1
		bge.s	Tails_BalanceOnObjRight
		bra.s	Tails_LookUp
; ---------------------------------------------------------------------------
; balancing checks for Tails
; loc_111A2:
Tails_Balance:
		jsr	(ObjHitFloor).l
		cmpi.w	#$C,d1
		blt.s	Tails_LookUp
		cmpi.b	#3,$36(a0)
		bne.s	Tails_BalanceLeft
; loc_111B6:
Tails_BalanceOnObjRight:
		bclr	#0,$22(a0)
		bra.s	Tails_BalanceDone
; ---------------------------------------------------------------------------
; loc_111BE:
Tails_BalanceLeft:
		cmpi.b	#3,$37(a0)
		bne.s	Tails_LookUp
; loc_111C6:
Tails_BalanceOnObjLeft:
		bset	#0,$22(a0)
; loc_111CC:
Tails_BalanceDone:
		move.b	#6,$1C(a0)
		bra.s	Obj02_UpdateSpeedOnGround
; ---------------------------------------------------------------------------
; loc_111d4:
Tails_LookUp:
		btst	#button_up,(Ctrl_2_Held).w	; is up being pressed?
		beq.s	Tails_Duck		; if not,branch
		move.b	#7,$1C(a0)		; use "looking up" animation
		bra.s	Obj02_UpdateSpeedOnGround
; ---------------------------------------------------------------------------
; loc_111E4:
Tails_Duck:
		btst	#button_down,(Ctrl_2_Held).w		; is down being pressed?
		beq.s	Obj02_UpdateSpeedOnGround	; if not,branch
		move.b	#8,$1C(a0)		; use "ducking" animation

; ---------------------------------------------------------------------------
; updates Tails' speed on the ground
; ---------------------------------------------------------------------------
; loc_111F2:
Obj02_UpdateSpeedOnGround:
		move.b	(Ctrl_2_Held).w,d0
		andi.b	#button_left_mask+button_right_mask,d0		; is left/right being pressed?
		bne.s	Obj02_Traction	; if yes,branch
		move.w	$14(a0),d0
		beq.s	Obj02_Traction
		bmi.s	Obj02_SettleLeft

; slow down when facing right and not pressing a direction
; Obj02_SettleRight:
		sub.w	d5,d0
		bhs.s	+
		move.w	#0,d0
+
		move.w	d0,$14(a0)
		bra.s	Obj02_Traction
; ---------------------------------------------------------------------------
; slow down when facing left and not pressing a direction
; loc_11212:
Obj02_SettleLeft:
		add.w	d5,d0
		bhs.s	+
		move.w	#0,d0
+
		move.w	d0,$14(a0)

; increase or decrease speed on the ground
; loc_1121E:
Obj02_Traction:
		move.b	$26(a0),d0
		jsr	(CalcSine).l
		muls.w	$14(a0),d1
		asr.l	#8,d1
		move.w	d1,$10(a0)
		muls.w	$14(a0),d0
		asr.l	#8,d0
		move.w	d0,$12(a0)

; stops Tails from running through walls that meet the ground
; loc_1123C:
Obj02_CheckWallsOnGround:
		move.b	$26(a0),d0
		addi.b	#$40,d0
		bmi.s	return_112AC
		move.b	#$40,d1
		tst.w	$14(a0)
		beq.s	return_112AC
		bmi.s	+
		neg.w	d1
+
		move.b	$26(a0),d0
		add.b	d1,d0
		move.w	d0,-(sp)
		bsr.w	Sonic_WalkSpeed
		move.w	(sp)+,d0
		tst.w	d1
		bpl.s	return_112AC
		asl.w	#8,d1
		addi.b	#$20,d0
		andi.b	#$C0,d0
		beq.s	loc_112A8
		cmpi.b	#$40,d0
		beq.s	loc_11296
		cmpi.b	#$80,d0
		beq.s	loc_11290
		add.w	d1,$10(a0)
		bset	#5,$22(a0)
		move.w	#0,$14(a0)
		rts
; ---------------------------------------------------------------------------

loc_11290:
		sub.w	d1,$12(a0)
		rts
; ---------------------------------------------------------------------------

loc_11296:
		sub.w	d1,$10(a0)
		bset	#5,$22(a0)
		move.w	#0,$14(a0)
		rts
; ---------------------------------------------------------------------------

loc_112A8:
		add.w	d1,$12(a0)

return_112AC:
		rts
; End of subroutine Tails_Move


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_112AE:
Tails_MoveLeft:
		move.w	$14(a0),d0
		beq.s	loc_112B6
		bpl.s	Tails_TurnLeft	; if Tails is already moving to the right,branch

loc_112B6:
		bset	#0,$22(a0)
		bne.s	loc_112CA
		bclr	#5,$22(a0)
		move.b	#1,$1D(a0)

loc_112CA:
		sub.w	d5,d0		; add acceleration to left
		move.w	d6,d1
		neg.w	d1
		cmp.w	d1,d0		; compare new speed with top speed
		bgt.s	loc_112DC	; if new speed is less than the maximum,branch
		add.w	d5,d0		; remove this frame's acceleration change
		cmp.w	d1,d0		; compare speed with top speed
		ble.s	loc_112DC	; if speed was already greater than the maximum,branc
		move.w	d1,d0		; limit speed on ground going left

loc_112DC:
		move.w	d0,$14(a0)
		move.b	#0,$1C(a0)	; use "walking" animation
		rts
; ---------------------------------------------------------------------------
; loc_112E8:
Tails_TurnLeft:
		sub.w	d4,d0
		bhs.s	loc_112F0
		move.w	#-$80,d0

loc_112F0:
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
		bne.s	return_1131E
		cmpi.w	#$400,d0
		blt.s	return_1131E
		move.b	#$D,$1C(a0)	; use "stopping" animation
		bclr	#0,$22(a0)
		move.w	#SndID_Skidding,d0		; use "stopping" sound
		jsr	(PlaySound).l

return_1131E:
		rts
; End of subroutine Tails_MoveLeft


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_11320:
Tails_MoveRight:
		move.w	$14(a0),d0
		bmi.s	Tails_TurnRight
		bclr	#0,$22(a0)
		beq.s	loc_1133A
		bclr	#5,$22(a0)
		move.b	#1,$1D(a0)	; force walking animation to restart if it's already in-progress

loc_1133A:
		add.w	d5,d0		; add acceleration to the right
		cmp.w	d6,d0		; compare new speed with top speed
		blt.s	loc_11348	; if new speed is less than the maximum,branch
		sub.w	d5,d0		; remove this frame's acceleration change
		cmp.w	d6,d0		; compare speed with top speed
		bge.s	loc_11348	; if speed was already greater than the maximum,branch
		move.w	d6,d0		; limit speed on ground going right

loc_11348:
		move.w	d0,$14(a0)
		move.b	#0,$1C(a0)	; use walking animation
		rts
; ---------------------------------------------------------------------------
; loc_11354:
Tails_TurnRight:
		add.w	d4,d0
		bhs.s	loc_1135C
		move.w	#$80,d0

loc_1135C:
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
		bne.s	return_1138A
		cmpi.w	#-$400,d0
		bgt.s	return_1138A
		move.b	#$D,$1C(a0)	; use "stopping" animation
		bset	#0,$22(a0)
		move.w	#SndID_Skidding,d0		; use "stopping" sound
		jsr	(PlaySound).l

return_1138A:
		rts
; End of subroutine Tails_MoveRight

; ---------------------------------------------------------------------------
; Subroutine to change Tails' speed as he rolls
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_1138C:
Tails_RollSpeed:
		move.w	(Sonic_top_speed).w,d6
		asl.w	#1,d6
		move.w	(Sonic_acceleration).w,d5
		asr.w	#1,d5			; natural roll deceleration = 1/2 normal acceleration
		; read Sonic's equivalent of the code for a hilarious screw-up
		move.w	(Sonic_deceleration).w,d4
		asr.w	#2,d4			; controlled roll deceleration
		tst.b	(Sliding_flag).w
		bne.w	Tails_SetRollSpeed
		tst.w	$2E(a0)
		bne.s	Tails_ApplyRollSpeed
		btst	#button_left,(Ctrl_2_Held).w	; is left being pressed?
		beq.s	loc_113B8		; if not,branch
		bsr.w	Tails_RollLeft

loc_113B8:
		btst	#button_right,(Ctrl_2_Held).w	; is right being pressed?
		beq.s	Tails_ApplyRollSpeed	; if not,branch
		bsr.w	Tails_RollRight
; loc_113C4:
Tails_ApplyRollSpeed:
		move.w	$14(a0),d0
		beq.s	Tails_CheckRollStop
		bmi.s	Tails_ApplyRollSpeedLeft

; Tails_ApplyRollSpeedRight:
		sub.w	d5,d0
		bhs.s	loc_113d4
		move.w	#0,d0

loc_113d4:
		move.w	d0,$14(a0)
		bra.s	Tails_CheckRollStop
; ---------------------------------------------------------------------------
; loc_113DA:
Tails_ApplyRollSpeedLeft:
		add.w	d5,d0
		bhs.s	loc_113E2
		move.w	#0,d0

loc_113E2:
		move.w	d0,$14(a0)
; loc_113E6:
Tails_CheckRollStop:
		tst.w	$14(a0)
		bne.s	Tails_SetRollSpeed
		bclr	#2,$22(a0)
		move.b	#$F,$16(a0)	; sets standing height to only slightly higher than rolling height,unlike Sonic
		move.b	#9,$17(a0)
		move.b	#5,$1C(a0)
		subq.w	#5,y_pos(a0)
; loc_11408:
Tails_SetRollSpeed:
		move.b	$26(a0),d0
		jsr	(CalcSine).l
		muls.w	$14(a0),d0
		asr.l	#8,d0
		move.w	d0,$12(a0)	; set y velocity based on $14 and angle
		muls.w	$14(a0),d1
		asr.l	#8,d1
		cmpi.w	#$1000,d1
		ble.s	loc_1142C
		move.w	#$1000,d1	; limit Tails' speed rolling right

loc_1142C:
		cmpi.w	#-$1000,d1
		bge.s	loc_11436
		move.w	#-$1000,d1	; limit Tails' speed rolling left

loc_11436:
		move.w	d1,$10(a0)	; set x velocity based on $14 and angle
		bra.w	Obj02_CheckWallsOnGround
; End of function Tails_RollSpeed


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_1143E:
Tails_RollLeft:
		move.w	$0014(a0),d0
		beq.s	loc_11446
		bpl.s	loc_11454
loc_11446:
		bset	#0,$0022(a0)
		move.b	#2,$001C(a0)
		rts
loc_11454:
		sub.w	d4,d0
		bhs.s	loc_1145C
		move.w	#-$80,d0
loc_1145C:
		move.w	d0,$0014(a0)
		rts
;===============================================================================
; Sub Routine Tails_RollLeft
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_RollRight
; [ Begin ]
;===============================================================================
Tails_RollRight: ; loc_11462:
		move.w	$0014(a0),d0
		bmi.s	loc_11476
		bclr	#0,$0022(a0)
		move.b	#2,$001C(a0)
		rts
loc_11476:
		add.w	d4,d0
		bhs.s	loc_1147E
		move.w	#$0080,d0
loc_1147E:
		move.w	d0,$0014(a0)
		rts
;===============================================================================
; Sub Routine Tails_RollRight
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_ChgJumpDir
; [ Begin ]
;===============================================================================
Tails_ChgJumpDir: ; loc_11484:
		move.w	(Sonic_top_speed).w,d6
		move.w	(Sonic_acceleration).w,d5
		asl.w	#1,d5
		btst	#$04,$0022(a0)
		bne.s	loc_114CE
		move.w	$0010(a0),d0
		btst	#button_left,(Ctrl_2_Held).w
		beq.s	loc_114B4
		bset	#0,$0022(a0)
		sub.w	d5,d0
		move.w	d6,d1
		neg.w	d1
		cmp.w	d1,d0
		bgt.s	loc_114B4
		move.w	d1,d0
loc_114B4:
		btst	#button_right,(Ctrl_2_Held).w
		beq.s	loc_114CA
		bclr	#0,$0022(a0)
		add.w	d5,d0
		cmp.w	d6,d0
		blt.s	loc_114CA
		move.w	d6,d0
loc_114CA:
		move.w	d0,$0010(a0)
loc_114CE:
		cmpi.w	#-$400,$0012(a0)
		blo.s	loc_114FC
		move.w	$0010(a0),d0
		move.w	d0,d1
		asr.w	#$05,d1
		beq.s	loc_114FC
		bmi.s	loc_114F0
		sub.w	d1,d0
		bhs.s	loc_114EA
		move.w	#0,d0
loc_114EA:
		move.w	d0,$0010(a0)
		rts
loc_114F0:
		sub.w	d1,d0
		blo.s	loc_114F8
		move.w	#0,d0
loc_114F8:
		move.w	d0,$0010(a0)
loc_114FC:
		rts
;===============================================================================
; Sub Routine Tails_ChgJumpDir
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_LevelBoundaries
; [ Begin ]
;===============================================================================
Tails_LevelBoundaries: ; loc_114FE:
		move.l	x_pos(a0),d1
		move.w	$0010(a0),d0
		ext.l	d0
		asl.l	#$08,d0
		add.l	d0,d1
		swap	d1
		move.w	(Camera_Min_X_pos).w,d0
		addi.w	#$0010,d0
		cmp.w	d1,d0
		bhi.s	loc_1156A
		move.w	(Camera_Max_X_pos).w,d0
		addi.w	#$0128,d0
		tst.b	(Current_Boss_ID).w
		bne.s	loc_1152C
		addi.w	#$0040,d0
loc_1152C:
		cmp.w	d1,d0
		bls.s	loc_1156A
loc_11530:
		move.w	(Camera_Max_Y_pos_now).w,d0
		addi.w	#$00E0,d0
		cmp.w	y_pos(a0),d0
		blt.s	loc_11540
		rts
loc_11540:

	if RemoveJmpTos
JmpTo2_KillCharacter
	endif
		jmpto	JmpTo2_KillCharacter				; loc_12074
		cmpi.w	#scrap_brain_zone_act_2,(Current_ZoneAndAct).w
		bne.w	JmpTo2_KillCharacter				 ; loc_12074
		cmpi.w	#$2000,x_pos(a0)
		blo.w	JmpTo2_KillCharacter				 ; loc_12074
		clr.b	(Last_star_pole_hit).w
		move.w	#$0001,(Level_Inactive_flag).w
		move.w	#labyrinth_zone_act_4,(Current_ZoneAndAct).w
		rts
loc_1156A:
		move.w	d0,x_pos(a0)
		move.w	#0,$000A(a0)
		move.w	#0,$0010(a0)
		move.w	#0,$0014(a0)
		bra.s	loc_11530
;===============================================================================
; Sub Routine Tails_LevelBoundaries
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_Roll
; [ Begin ]
;===============================================================================
Tails_Roll: ; loc_11582:
		tst.b	(Sliding_flag).w
		bne.s	loc_115A8
		move.w	$0014(a0),d0
		bpl.s	loc_11590
		neg.w	d0
loc_11590:
		cmpi.w	#$0080,d0
		blo.s	loc_115A8
		move.b	(Ctrl_2_Held).w,d0
		andi.b	#button_left_mask+button_right_mask,d0
		bne.s	loc_115A8
		btst	#button_down,(Ctrl_2_Held).w
		bne.s	loc_115AA
loc_115A8:
		rts
loc_115AA:
		btst	#2,$0022(a0)
		beq.s	loc_115B4
		rts
loc_115B4:
		bset	#2,$0022(a0)
		move.b	#$0E,$0016(a0)
		move.b	#$07,$0017(a0)
		move.b	#2,$001C(a0)
		addq.w	#$05,y_pos(a0)
		move.w	#SndID_Roll,d0
		jsr	(PlaySound).l			  ; loc_14C6
		tst.w	$0014(a0)
		bne.s	loc_115E6
		move.w	#$0200,$0014(a0)
loc_115E6:
		rts
;===============================================================================
; Sub Routine Tails_Roll
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_Jump
; [ Begin ]
;===============================================================================
Tails_Jump: ; loc_115E8:
		move.b	(Ctrl_2_Press).w,d0
		andi.b	#button_A_mask+button_B_mask+button_C_mask,d0
		beq.w	loc_1168C
		moveq	#0,d0
		move.b	$0026(a0),d0
		addi.b	#$80,d0
		bsr.w	loc_136F2
		cmpi.w	#$0006,d1
		blt.w	loc_1168C
		move.w	#$0680,d2
		btst	#$06,$0022(a0)
		beq.s	loc_1161A
		move.w	#$0380,d2
loc_1161A:
		moveq	#0,d0
		move.b	$0026(a0),d0
		subi.b	#$40,d0
		jsr	(CalcSine).l		; loc_320A
		muls.w	d2,d1
		asr.l	#$08,d1
		add.w	d1,$0010(a0)
		muls.w	d2,d0
		asr.l	#$08,d0
		add.w	d0,$0012(a0)
		bset	#1,$0022(a0)
		bclr	#$05,$0022(a0)
		addq.l	#$04,sp
		move.b	#1,$003C(a0)
		clr.b	$0038(a0)
		move.w	#SndID_Jump,d0
		jsr	(PlaySound).l			  ; loc_14C6
		move.b	#$0F,$0016(a0)
		move.b	#$09,$0017(a0)
		btst	#2,$0022(a0)
		bne.s	loc_1168E
		move.b	#$0E,$0016(a0)
		move.b	#$07,$0017(a0)
		move.b	#2,$001C(a0)
		bset	#2,$0022(a0)
		addq.w	#$05,y_pos(a0)
loc_1168C:
		rts
loc_1168E:
		bset	#$04,$0022(a0)
		rts
;===============================================================================
; Sub Routine Tails_Jump
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_JumpHeight
; [ Begin ]
;===============================================================================
Tails_JumpHeight: ; loc_11696:
		tst.b	$003C(a0)
		beq.s	loc_116C2
		move.w	#-$400,d1
		btst	#$06,$0022(a0)
		beq.s	loc_116AC
		move.w	#-$200,d1
loc_116AC:
		cmp.w	$0012(a0),d1
		ble.s	loc_116C0
		move.b	(Ctrl_2_Held).w,d0
		andi.b	#button_A_mask+button_B_mask+button_C_mask,d0
		bne.s	loc_116C0
		move.w	d1,$0012(a0)
loc_116C0:
		rts
loc_116C2:
		cmpi.w	#$F040,$0012(a0)
		bge.s	loc_116d0
		move.w	#$F040,$0012(a0)
loc_116d0:
		rts
;===============================================================================
; Sub Routine Tails_JumpHeight
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_Spindash
; [ Begin ]
;===============================================================================
Tails_Spindash: ; loc_116d2:
		tst.b	$0039(a0)
		bne.s	loc_11706
		cmpi.b	#$08,$001C(a0)
		bne.s	loc_11704
		move.b	(Ctrl_2_Press).w,d0
		andi.b	#button_A_mask+button_B_mask+button_C_mask,d0
		beq.w	loc_11704
		move.b	#$09,$001C(a0)
		move.w	#SndID_Roll,d0
		jsr	(PlaySound).l			  ; loc_14C6
		addq.l	#$04,sp
		move.b	#1,$0039(a0)
loc_11704:
		rts
loc_11706:
		move.b	(Ctrl_2_Held).w,d0
		btst	#button_down,d0
		bne.s	loc_1174C
		move.b	#$0E,$0016(a0)
		move.b	#$07,$0017(a0)
		move.b	#2,$001C(a0)
		addq.w	#$05,y_pos(a0)
		move.b	#0,$0039(a0)
		move.w	#$2000,(Horiz_scroll_delay_val_P2).w
		move.w	#$0800,$0014(a0)
		btst	#0,$0022(a0)
		beq.s	loc_11744
		neg.w	$0014(a0)
loc_11744:
		bset	#2,$0022(a0)
		rts
loc_1174C:
		move.b	(Ctrl_2_Press).w,d0
		andi.b	#button_A_mask+button_B_mask+button_C_mask,d0
		beq.w	loc_1175A
		nop
loc_1175A:
		addq.l	#$04,sp
		rts
;===============================================================================
; Sub Routine Tails_Spindash
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_SlopeResist
; [ Begin ]
;===============================================================================
Tails_SlopeResist: ; loc_1175E:
		move.b	$0026(a0),d0
		addi.b	#$60,d0
		cmpi.b	#$C0,d0
		bhs.s	loc_11792
		move.b	$0026(a0),d0
		jsr	(CalcSine).l		; loc_320A
		muls.w	#$0020,d0
		asr.l	#$08,d0
		tst.w	$0014(a0)
		beq.s	loc_11792
		bmi.s	loc_1178E
		tst.w	d0
		beq.s	loc_1178C
		add.w	d0,$0014(a0)
loc_1178C:
		rts
loc_1178E:
		add.w	d0,$0014(a0)
loc_11792:
		rts
;===============================================================================
; Sub Routine Tails_SlopeResist
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_RollRepel
; [ Begin ]
;===============================================================================
Tails_RollRepel: ; loc_11794:
		move.b	$0026(a0),d0
		addi.b	#$60,d0
		cmpi.b	#$C0,d0
		bhs.s	loc_117CE
		move.b	$0026(a0),d0
		jsr	(CalcSine).l		; loc_320A
		muls.w	#$0050,d0
		asr.l	#$08,d0
		tst.w	$0014(a0)
		bmi.s	loc_117C4
		tst.w	d0
		bpl.s	loc_117BE
		asr.l	#2,d0
loc_117BE:
		add.w	d0,$0014(a0)
		rts
loc_117C4:
		tst.w	d0
		bmi.s	loc_117CA
		asr.l	#2,d0
loc_117CA:
		add.w	d0,$0014(a0)
loc_117CE:
		rts
;===============================================================================
; Sub Routine Tails_RollRepel
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_SlopeRepel
; [ Begin ]
;===============================================================================
Tails_SlopeRepel: ; loc_117d0:
		nop
		tst.b	$0038(a0)
		bne.s	loc_1180A
		tst.w	$002E(a0)
		bne.s	loc_1180C
		move.b	$0026(a0),d0
		addi.b	#$20,d0
		andi.b	#$C0,d0
		beq.s	loc_1180A
		move.w	$0014(a0),d0
		bpl.s	loc_117F4
		neg.w	d0
loc_117F4:
		cmpi.w	#$0280,d0
		bhs.s	loc_1180A
		clr.w	$0014(a0)
		bset	#1,$0022(a0)
		move.w	#$001E,$002E(a0)
loc_1180A:
		rts
loc_1180C:
		subq.w	#1,$002E(a0)
		rts
;===============================================================================
; Sub Routine Tails_SlopeRepel
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_JumpAngle
; [ Begin ]
;===============================================================================
Tails_JumpAngle: ; loc_11812:
		move.b	$0026(a0),d0
		beq.s	loc_1182C
		bpl.s	loc_11822
		addq.b	#2,d0
		bhs.s	loc_11820
		moveq	#0,d0
loc_11820:
		bra.s	loc_11828
loc_11822:
		subq.b	#2,d0
		bhs.s	loc_11828
		moveq	#0,d0
loc_11828:
		move.b	d0,$0026(a0)
loc_1182C:
		move.b	$0027(a0),d0
		beq.s	loc_11870
		tst.w	$0014(a0)
		bmi.s	loc_11850
loc_11838:
		move.b	$002D(a0),d1
		add.b	d1,d0
		bhs.s	loc_1184E
		subq.b	#1,$002C(a0)
		bhs.s	loc_1184E
		move.b	#0,$002C(a0)
		moveq	#0,d0
loc_1184E:
		bra.s	loc_1186C
loc_11850:
		tst.b	$0029(a0)
		bne.s	loc_11838
		move.b	$002D(a0),d1
		sub.b	d1,d0
		bhs.s	loc_1186C
		subq.b	#1,$002C(a0)
		bhs.s	loc_1186C
		move.b	#0,$002C(a0)
		moveq	#0,d0
loc_1186C:
		move.b	d0,$0027(a0)
loc_11870:
		rts
;===============================================================================
; Sub Routine Tails_JumpAngle
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_Floor
; [ Begin ]
;===============================================================================
Tails_Floor: ; loc_11872:
		move.b	$003F(a0),d5
		move.w	$0010(a0),d1
		move.w	$0012(a0),d2
		jsr	(CalcAngle).l			   ; loc_34A2
		move.b	d0,$002B(a0)
		subi.b	#$20,d0
		andi.b	#$C0,d0
		cmpi.b	#$40,d0
		beq.w	loc_11946
		cmpi.b	#$80,d0
		beq.w	loc_119A8
		cmpi.b	#$C0,d0
		beq.w	loc_11A04
		bsr.w	Sonic_HitWall			; loc_13AFC
		tst.w	d1
		bpl.s	loc_118BA
		sub.w	d1,x_pos(a0)
		move.w	#0,$0010(a0)
loc_118BA:
		bsr.w	loc_1397A
		tst.w	d1
		bpl.s	loc_118CC
		add.w	d1,x_pos(a0)
		move.w	#0,$0010(a0)
loc_118CC:
		bsr.w	loc_13736
		tst.w	d1
		bpl.s	loc_11944
		move.b	$0012(a0),d2
		addq.b	#$08,d2
		neg.b	d2
		cmp.b	d2,d1
		bge.s	loc_118E4
		cmp.b	d2,d0
		blt.s	loc_11944
loc_118E4:
		add.w	d1,y_pos(a0)
		move.b	d3,$0026(a0)
		bsr.w	Tails_ResetTailsOnFloor ; loc_11A66
		move.b	#0,$001C(a0)
		move.b	d3,d0
		addi.b	#$20,d0
		andi.b	#$40,d0
		bne.s	loc_11922
		move.b	d3,d0
		addi.b	#$10,d0
		andi.b	#$20,d0
		beq.s	loc_11914
		asr.w	$0012(a0)
		bra.s	loc_11936
loc_11914:
		move.w	#0,$0012(a0)
		move.w	$0010(a0),$0014(a0)
		rts
loc_11922:
		move.w	#0,$0010(a0)
		cmpi.w	#$0FC0,$0012(a0)
		ble.s	loc_11936
		move.w	#$0FC0,$0012(a0)
loc_11936:
		move.w	$0012(a0),$0014(a0)
		tst.b	d3
		bpl.s	loc_11944
		neg.w	$0014(a0)
loc_11944:
		rts
loc_11946:
		bsr.w	Sonic_HitWall			; loc_13AFC
		tst.w	d1
		bpl.s	loc_11960
		sub.w	d1,x_pos(a0)
		move.w	#0,$0010(a0)
		move.w	$0012(a0),$0014(a0)
		rts
loc_11960:
		bsr.w	Sonic_DontRunOnWalls	; loc_139CC
		tst.w	d1
		bpl.s	loc_1197A
		sub.w	d1,y_pos(a0)
		tst.w	$0012(a0)
		bpl.s	loc_11978
		move.w	#0,$0012(a0)
loc_11978:
		rts
loc_1197A:
		tst.w	$0012(a0)
		bmi.s	loc_119A6
		bsr.w	loc_13736
		tst.w	d1
		bpl.s	loc_119A6
		add.w	d1,y_pos(a0)
		move.b	d3,$0026(a0)
		bsr.w	Tails_ResetTailsOnFloor ; loc_11A66
		move.b	#0,$001C(a0)
		move.w	#0,$0012(a0)
		move.w	$0010(a0),$0014(a0)
loc_119A6:
		rts
loc_119A8:
		bsr.w	Sonic_HitWall			; loc_13AFC
		tst.w	d1
		bpl.s	loc_119BA
		sub.w	d1,x_pos(a0)
		move.w	#0,$0010(a0)
loc_119BA:
		bsr.w	loc_1397A
		tst.w	d1
		bpl.s	loc_119CC
		add.w	d1,x_pos(a0)
		move.w	#0,$0010(a0)
loc_119CC:
		bsr.w	Sonic_DontRunOnWalls	; loc_139CC
		tst.w	d1
		bpl.s	loc_11A02
		sub.w	d1,y_pos(a0)
		move.b	d3,d0
		addi.b	#$20,d0
		andi.b	#$40,d0
		bne.s	loc_119EC
		move.w	#0,$0012(a0)
		rts
loc_119EC:
		move.b	d3,$0026(a0)
		bsr.w	Tails_ResetTailsOnFloor ; loc_11A66
		move.w	$0012(a0),$0014(a0)
		tst.b	d3
		bpl.s	loc_11A02
		neg.w	$0014(a0)
loc_11A02:
		rts
loc_11A04:
		bsr.w	loc_1397A
		tst.w	d1
		bpl.s	loc_11A1E
		add.w	d1,x_pos(a0)
		move.w	#0,$0010(a0)
		move.w	$0012(a0),$0014(a0)
		rts
loc_11A1E:
		bsr.w	Sonic_DontRunOnWalls	; loc_139CC
		tst.w	d1
		bpl.s	loc_11A38
		sub.w	d1,y_pos(a0)
		tst.w	$0012(a0)
		bpl.s	loc_11A36
		move.w	#0,$0012(a0)
loc_11A36:
		rts
loc_11A38:
		tst.w	$0012(a0)
		bmi.s	loc_11A64
		bsr.w	loc_13736
		tst.w	d1
		bpl.s	loc_11A64
		add.w	d1,y_pos(a0)
		move.b	d3,$0026(a0)
		bsr.w	Tails_ResetTailsOnFloor ; loc_11A66
		move.b	#0,$001C(a0)
		move.w	#0,$0012(a0)
		move.w	$0010(a0),$0014(a0)
loc_11A64:
		rts
;===============================================================================
; Sub Routine Tails_Floor
; [ End ]
;===============================================================================

;===============================================================================
; Object 0x02 - Tails
; [ End ]
;===============================================================================

;===============================================================================
; Sub Routine Tails_ResetTailsOnFloor
; [ Begin ]
;===============================================================================
Tails_ResetTailsOnFloor: ; loc_11A66:
		btst	#$04,$0022(a0)
		beq.s	loc_11A74
		nop
		nop
		nop
loc_11A74:
		bclr	#$05,$0022(a0)
		bclr	#1,$0022(a0)
		bclr	#$04,$0022(a0)
		btst	#2,$0022(a0)
		beq.s	loc_11AAA
		bclr	#2,$0022(a0)
		move.b	#$0F,$0016(a0)
		move.b	#$09,$0017(a0)
		move.b	#0,$001C(a0)
		subq.w	#1,y_pos(a0)
loc_11AAA:
		move.b	#0,$003C(a0)
		move.w	#0,(Chain_Bonus_counter).w
		move.b	#0,$0027(a0)
		move.b	#0,$0029(a0)
		rts
;===============================================================================
; Sub Routine Tails_ResetTailsOnFloor
; [ End ]
;===============================================================================

Tails_Hurt: ; loc_11AC4:
		jsr	(ObjectMove).l				 ; loc_d27A
		addi.w	#$0030,$0012(a0)
		btst	#$06,$0022(a0)
		beq.s	loc_11ADE
		subi.w	#$0020,$0012(a0)
loc_11ADE:
		bsr.w	Tails_HurtStop			; loc_11AF4
		bsr.w	Tails_LevelBoundaries	; loc_114FE
		bsr.w	Tails_Animate			; loc_11BA2
		bsr.w	LoadTailsDynPLC	 ; loc_11F42
		jmp	(DisplaySprite).l			; loc_d3C2

;===============================================================================
; Sub Routine Tails_HurtStop
; [ Begin ]
;===============================================================================
Tails_HurtStop: ; loc_11AF4:
		move.w	(Camera_Max_Y_pos_now).w,d0
		addi.w	#$00E0,d0
		cmp.w	y_pos(a0),d0
		blo.w	JmpTo2_KillCharacter				 ; loc_12074
		bsr.w	Tails_Floor				; loc_11872
		btst	#1,$0022(a0)
		bne.s	loc_11B30
		moveq	#0,d0
		move.w	d0,$0012(a0)
		move.w	d0,$0010(a0)
		move.w	d0,$0014(a0)
		move.b	#0,$001C(a0)
		move.b	#2,$0024(a0)
		move.w	#$0078,$0030(a0)
loc_11B30:
		rts
;===============================================================================
; Sub Routine Tails_HurtStop
; [ End ]
;===============================================================================

Tails_Death: ; loc_11B32:
		bsr.w	Tails_GameOver			; loc_11B4A
		jsr	(ObjectMoveAndFall).l				; loc_d24E
		bsr.w	Tails_Animate			; loc_11BA2
		bsr.w	LoadTailsDynPLC	 ; loc_11F42
		jmp	(DisplaySprite).l			; loc_d3C2

;===============================================================================
; Sub Routine Tails_GameOver
; [ Begin ]
;===============================================================================
Tails_GameOver: ; loc_11B4A:
		move.w	(Camera_Max_Y_pos_now).w,d0
		addi.w	#$0100,d0
		cmp.w	y_pos(a0),d0
		bhs.w	loc_11B8C
		move.w	(MainCharacter+x_pos).w,d0
		subi.w	#$0040,d0
		move.w	d0,x_pos(a0)
		move.w	(MainCharacter+$C).w,d0
		subi.w	#$0080,d0
		move.w	d0,y_pos(a0)
		move.b	#2,$0024(a0)
		andi.w	#$7FFF,art_tile(a0)
		move.b	#$0C,$003E(a0)
		move.b	#$0D,$003F(a0)
		nop
loc_11B8C:
		rts
;===============================================================================
; Sub Routine Tails_GameOver
; [ End ]
;===============================================================================

Tails_ResetLevel: ; loc_11B8E:
		tst.w	$003A(a0)
		beq.s	loc_11BA0
		subq.w	#1,$003A(a0)
		bne.s	loc_11BA0
		move.w	#$0001,(Level_Inactive_flag).w
loc_11BA0:
		rts

;===============================================================================
; Sub Routine Tails_Animate
; [ Begin ]
;===============================================================================
Tails_Animate: ; loc_11BA2: ; Tails Subroutine
		lea	(Tails_AnimateData).l,A1 ; loc_11DF4
Tails_Animate2: ; loc_11BA8:
		moveq	#0,d0
		move.b	$001C(a0),d0
		cmp.b	$001D(a0),d0
		beq.s	loc_11BCA
		move.b	d0,$001D(a0)
		move.b	#0,$001B(a0)
		move.b	#0,$001E(a0)
		bclr	#$05,$0022(a0)
loc_11BCA:
		add.w	d0,d0
		adda.w	(A1,d0.w),A1
		move.b	(a1),d0
		bmi.s	loc_11C3A
		move.b	$0022(a0),d1
		andi.b	#1,d1
		andi.b	#$FC,$0001(a0)
		or.b	d1,$0001(a0)
		subq.b	#1,$001E(a0)
		bpl.s	loc_11C08
		move.b	d0,$001E(a0)
loc_11BF0:
		moveq	#0,d1
		move.b	$001B(a0),d1
		move.b	$01(A1,d1),d0
		cmpi.b	#$F0,d0
		bhs.s	loc_11C0A
loc_11C00:
		move.b	d0,$001A(a0)
		addq.b	#1,$001B(a0)
loc_11C08:
		rts
loc_11C0A:
		addq.b	#1,d0
		bne.s	loc_11C1A
		move.b	#0,$001B(a0)
		move.b	$0001(a1),d0
		bra.s	loc_11C00
loc_11C1A:
		addq.b	#1,d0
		bne.s	loc_11C2E
		move.b	$02(A1,d1),d0
		sub.b	d0,$001B(a0)
		sub.b	d0,d1
		move.b	$01(A1,d1),d0
		bra.s	loc_11C00
loc_11C2E:
		addq.b	#1,d0
		bne.s	loc_11C38
		move.b	$02(A1,d1),$001C(a0)
loc_11C38:
		rts
loc_11C3A:
		subq.b	#1,$001E(a0)
		bpl.s	loc_11C08
		addq.b	#1,d0
		bne.w	loc_11d26
		moveq	#0,d0
		move.b	$0027(a0),d0
		bne.w	loc_11CC0
		moveq	#0,d1
		move.b	$0026(a0),d0
		move.b	$0022(a0),d2
		andi.b	#1,d2
		bne.s	loc_11C62
		not.b	d0
loc_11C62:
		addi.b	#$10,d0
		bpl.s	loc_11C6A
		moveq	#$03,d1
loc_11C6A:
		andi.b	#$FC,$0001(a0)
		eor.b	d1,d2
		or.b	d2,$0001(a0)
		lsr.b	#$04,d0
		andi.b	#$06,d0
		move.w	$0014(a0),d2
		bpl.s	loc_11C84
		neg.w	d2
loc_11C84:
		move.b	d0,d3
		add.b	d3,d3
		add.b	d3,d3
		lea	(Tails_Animate_Walk).l,A1 ; loc_11E32
		cmpi.w	#$0600,d2
		blo.s	loc_11CA6
		lea	(Tails_Animate_Run).l,A1 ; loc_11E3C
		move.b	d0,d1
		lsr.b	#1,d1
		add.b	d1,d0
		add.b	d0,d0
		move.b	d0,d3
loc_11CA6:
		neg.w	d2
		addi.w	#$0800,d2
		bpl.s	loc_11CB0
		moveq	#0,d2
loc_11CB0:
		lsr.w	#$08,d2
		move.b	d2,$001E(a0)
		bsr.w	loc_11BF0
		add.b	d3,$001A(a0)
		rts
loc_11CC0:
		move.b	$0027(a0),d0
		moveq	#0,d1
		move.b	$0022(a0),d2
		andi.b	#1,d2
		bne.s	loc_11CEE
		andi.b	#$FC,$0001(a0)
		addi.b	#$0B,d0
		divu.w	#$0016,d0
		addi.b	#$75,d0
		move.b	d0,$001A(a0)
		move.b	#0,$001E(a0)
		rts
loc_11CEE:
		andi.b	#$FC,$0001(a0)
		tst.b	$0029(a0)
		beq.s	loc_11d06
		ori.b	#1,$0001(a0)
		addi.b	#$0B,d0
		bra.s	loc_11d12
loc_11d06:
		ori.b	#$03,$0001(a0)
		neg.b	d0
		addi.b	#$8F,d0
loc_11d12:
		divu.w	#$0016,d0
		addi.b	#$75,d0
		move.b	d0,$001A(a0)
		move.b	#0,$001E(a0)
		rts
loc_11d26:
		addq.b	#1,d0
		bne.s	loc_11d6A
		move.w	$0014(a0),d2
		bpl.s	loc_11d32
		neg.w	d2
loc_11d32:
		lea	(Tails_Animate_Roll2).l,A1 ; loc_11E4B
		cmpi.w	#$0600,d2
		bhs.s	loc_11d44
		lea	(Tails_Animate_Roll).l,A1 ; loc_11E46
loc_11d44:
		neg.w	d2
		addi.w	#$0400,d2
		bpl.s	loc_11d4E
		moveq	#0,d2
loc_11d4E:
		lsr.w	#$08,d2
		move.b	d2,$001E(a0)
		move.b	$0022(a0),d1
		andi.b	#1,d1
		andi.b	#$FC,$0001(a0)
		or.b	d1,$0001(a0)
		bra.w	loc_11BF0
loc_11d6A:
		addq.b	#1,d0
		bne.s	loc_11DA0
		move.w	$0014(a0),d2
		bmi.s	loc_11d76
		neg.w	d2
loc_11d76:
		addi.w	#$0800,d2
		bpl.s	loc_11d7E
		moveq	#0,d2
loc_11d7E:
		lsr.w	#$06,d2
		move.b	d2,$001E(a0)
		lea	(Tails_Animate_Push_NoArt).l,A1 ; loc_11E50
		move.b	$0022(a0),d1
		andi.b	#1,d1
		andi.b	#$FC,$0001(a0)
		or.b	d1,$0001(a0)
		bra.w	loc_11BF0
loc_11DA0:
		move.w	(Sidekick+$10).w,d1
		move.w	(Sidekick+$12).w,d2
		jsr	(CalcAngle).l			   ; loc_34A2
		moveq	#0,d1
		move.b	$0022(a0),d2
		andi.b	#1,d2
		bne.s	loc_11DBE
		not.b  d0
		bra.s	loc_11DC2
loc_11DBE:
		addi.b	#$80,d0
loc_11DC2:
		addi.b	#$10,d0
		bpl.s	loc_11DCA
		moveq	#$03,d1
loc_11DCA:
		andi.b	#$FC,$0001(a0)
		eor.b	d1,d2
		or.b	d2,$0001(a0)
		lsr.b	#$03,d0
		andi.b	#$0C,d0
		move.b	d0,d3
		lea	(loc_12054).l,A1
		move.b	#$03,$001E(a0)
		bsr.w	loc_11BF0
		add.b	d3,$001A(a0)
		rts
Tails_AnimateData: ; loc_11DF4: ; Tails Data
		dc.w	Tails_Animate_Walk-Tails_AnimateData		   ; loc_11E32
		dc.w	Tails_Animate_Run-Tails_AnimateData			   ; loc_11E3C
		dc.w	Tails_Animate_Roll-Tails_AnimateData		   ; loc_11E46
		dc.w	Tails_Animate_Roll2-Tails_AnimateData		   ; loc_11E4B
		dc.w	Tails_Animate_Push_NoArt-Tails_AnimateData	   ; loc_11E50
		dc.w	Tails_Animate_Wait-Tails_AnimateData		   ; loc_11E58
		dc.w	Tails_Animate_Balance_NoArt-Tails_AnimateData  ; loc_11E96
		dc.w	Tails_Animate_LookUp-Tails_AnimateData		   ; loc_11EA0
		dc.w	Tails_Animate_Duck-Tails_AnimateData		   ; loc_11EA3
		dc.w	Tails_Animate_Spindash-Tails_AnimateData	   ; loc_11EA6
		dc.w	Tails_Animate_0x0A-Tails_AnimateData		   ; loc_11EAB
		dc.w	Tails_Animate_0x0B-Tails_AnimateData		   ; loc_11EAE
		dc.w	Tails_Animate_0x0C-Tails_AnimateData		   ; loc_11EB4
		dc.w	Tails_Animate_Stop-Tails_AnimateData		   ; loc_11EB8
		dc.w	Tails_Animate_Fly-Tails_AnimateData			   ; loc_11EBC
		dc.w	Tails_Animate_0x0F-Tails_AnimateData		   ; loc_11EC0
		dc.w	Tails_Animate_Jump-Tails_AnimateData		   ; loc_11EC7
		dc.w	Tails_Animate_0x11-Tails_AnimateData		   ; loc_11Ed6
		dc.w	Tails_Animate_0x12-Tails_AnimateData		   ; loc_11EDA
		dc.w	Tails_Animate_0x13-Tails_AnimateData		   ; loc_11EE0
		dc.w	Tails_Animate_0x14-Tails_AnimateData		   ; loc_11EE5
		dc.w	Tails_Animate_0x15-Tails_AnimateData		   ; loc_11EE8
		dc.w	Tails_Animate_Death1-Tails_AnimateData		   ; loc_11EEF
		dc.w	Tails_Animate_Unused_Drown-Tails_AnimateData   ; loc_11EF2
		dc.w	Tails_Animate_Death2-Tails_AnimateData		   ; loc_11EF5
		dc.w	Tails_Animate_0x19-Tails_AnimateData		   ; loc_11EF8
		dc.w	Tails_Animate_0x1A-Tails_AnimateData		   ; loc_11EFB
		dc.w	Tails_Animate_0x1B-Tails_AnimateData		   ; loc_11EFE
		dc.w	Tails_Animate_0x1C-Tails_AnimateData		   ; loc_11F02
		dc.w	Tails_Animate_0x1D-Tails_AnimateData		   ; loc_11F06
		dc.w	Tails_Animate_0x1E-Tails_AnimateData		   ; loc_11F10
Tails_Animate_Walk: ; loc_11E32:
		dc.b	$FF,$10,$11,$12,$13,$14,$15,$0E,$0F,$FF
Tails_Animate_Run: ; loc_11E3C:
		dc.b	$FF,$2E,$2F,$30,$31,$FF,$FF,$FF,$FF,$FF
Tails_Animate_Roll: ; loc_11E46:
		dc.b	$01,$48,$47,$46,$FF
Tails_Animate_Roll2: ; loc_11E4B:
		dc.b	$01,$48,$47,$46,$FF
Tails_Animate_Push_NoArt: ; loc_11E50:
		dc.b	$FD,$09,$0A,$0B,$0C,$0D,$0E,$FF
Tails_Animate_Wait: ; loc_11E58:
		dc.b	$07,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$02,$01,$01,$01
		dc.b	$01,$01,$01,$01,$01,$03,$02,$01,$01,$01,$01,$01,$01,$01,$01,$01
		dc.b	$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
		dc.b	$06,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$06,$FE,$1C
Tails_Animate_Balance_NoArt: ; loc_11E96:
		dc.b	$1F,$01,$02,$03,$04,$05,$06,$07,$08,$FF
Tails_Animate_LookUp: ; loc_11EA0:
		dc.b	$3F,$04,$FF
Tails_Animate_Duck: ; loc_11EA3:
		dc.b	$3F,$5B,$FF
Tails_Animate_Spindash: ; loc_11EA6:
		dc.b	$00,$60,$61,$62,$FF
Tails_Animate_0x0A: ; loc_11EAB:
		dc.b	$3F,$82,$FF
Tails_Animate_0x0B: ; loc_11EAE:
		dc.b	$07,$08,$08,$09,$FD,$05
Tails_Animate_0x0C: ; loc_11EB4:
		dc.b	$07,$09,$FD,$05
Tails_Animate_Stop: ; loc_11EB8:
		dc.b	$07,$01,$02,$FF
Tails_Animate_Fly: ; loc_11EBC:
		dc.b	$07,$5E,$5F,$FF
Tails_Animate_0x0F: ; loc_11EC0:
		dc.b	$07,$01,$02,$03,$04,$05,$FF
Tails_Animate_Jump: ; loc_11EC7:
		dc.b	$03,$59,$5A,$59,$5A,$59,$5A,$59,$5A,$59,$5A,$59,$5A,$FD,$00
Tails_Animate_0x11: ; loc_11Ed6:
		dc.b	$04,$01,$02,$FF
Tails_Animate_0x12: ; loc_11EDA:
		dc.b	$0F,$01,$02,$03,$FE,$01
Tails_Animate_0x13: ; loc_11EE0:
		dc.b	$0F,$01,$02,$FE,$01
Tails_Animate_0x14: ; loc_11EE5:
		dc.b	$3F,$01,$FF
Tails_Animate_0x15: ; loc_11EE8:
		dc.b	$0B,$01,$02,$03,$04,$FD,$00
Tails_Animate_Death1: ; loc_11EEF:
		dc.b	$20,$5D,$FF
Tails_Animate_Unused_Drown: ; loc_11EF2:
		dc.b	$2F,$5D,$FF
Tails_Animate_Death2: ; loc_11EF5:
		dc.b	$03,$5D,$FF
Tails_Animate_0x19: ; loc_11EF8:
		dc.b	$03,$5D,$FF
Tails_Animate_0x1A: ; loc_11EFB:
		dc.b	$03,$5C,$FF
Tails_Animate_0x1B: ; loc_11EFE:
		dc.b	$07,$01,$01,$FF
Tails_Animate_0x1C: ; loc_11F02:
		dc.b	$77,$00,$FD,$00
Tails_Animate_0x1D: ; loc_11F06:
		dc.b	$03,$01,$02,$03,$04,$05,$06,$07,$08,$FF
Tails_Animate_0x1E: ; loc_11F10:
		dc.b	$03,$01,$02,$03,$04,$05,$06,$07,$08,$FF
		even

; ===========================================================================
; ---------------------------------------------------------------------------
; Tails' Tails pattern loading subroutine
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


; loc_11F1A: Load_Tails_Tail_Dynamic_PLC:
LoadTailsTailsDynPLC:
		moveq	#0,d0
		move.b	$1A(a0),d0
		cmp.b	(TailsTails_LastLoadedDPLC).w,d0
		beq.s	return_11F94
		move.b	d0,(TailsTails_LastLoadedDPLC).w
		lea	(Tails_Dyn_Script).l,a2
		add.w	d0,d0
		adda.w	(a2,d0.w),a2
		move.w	(a2)+,d5
		subq.w	#1,d5
		bmi.s	return_11F94
		move.w	#$F600,d4
		bra.s	TPLC_ReadEntry

; ---------------------------------------------------------------------------
; Tails pattern loading subroutine
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_11F42: Load_Tails_Dynamic_PLC:
LoadTailsDynPLC:
		moveq	#0,d0
		move.b	$1A(a0),d0
		cmp.b	(Tails_LastLoadedDPLC).w,d0
		beq.s	return_11F94
		move.b	d0,(Tails_LastLoadedDPLC).w
		lea	(Tails_Dyn_Script).l,a2
		add.w	d0,d0
		adda.w	(a2,d0.w),a2
		move.w	(a2)+,d5
		subq.w	#1,d5
		bmi.s	return_11F94
		move.w	#$F400,d4
; loc_11F68:
TPLC_ReadEntry:
		moveq	#0,d1
		move.w	(a2)+,d1
		move.w	d1,d3
		lsr.w	#8,d3
		andi.w	#$F0,d3
		addi.w	#$10,d3
		andi.w	#$FFF,d1
		lsl.l	#5,d1
		addi.l	#Tails_Sprites,d1
		move.w	d4,d2
		add.w	d3,d4
		add.w	d3,d4
		jsr	(QueueDMATransfer).l
		dbf	d5,TPLC_ReadEntry

return_11F94:
		rts
; End of function LoadTailsDynPLC