; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Equates section - Names for variables.

; ---------------------------------------------------------------------------
; size variables - you'll get an informational error if you need to change these...
; they are all in units of bytes
Size_of_DAC_samples =		$2723
Size_of_SEGA_sound =		$6174
	if FixBugs
; To be on the safe side, we'll use a larger guess size.
Size_of_Snd_driver_guess =	$E80 ; approximate post-compressed size of the Z80 sound driver
	else
Size_of_Snd_driver_guess =	$DF3 ; approximate post-compressed size of the Z80 sound driver
	endif

; ---------------------------------------------------------------------------
; Object Status Table offsets (for everything between Object_RAM and Primary_Collision)
; ---------------------------------------------------------------------------
; universally followed object conventions:
id =			  0 ; object ID (if you change this, change insn1op and insn2op in s2.macrosetup.asm, if you still use them)
render_flags =		  1 ; bitfield ; bit 7 = onscreen flag, bit 0 = x mirror, bit 1 = y mirror, bit 2 = coordinate system, bit 6 = render subobjects
art_tile =		  2 ; and 3 ; start of sprite's art
mappings =		  4 ; and 5 and 6 and 7
x_pos =			  8 ; and 9 ... some objects use $A and $B as well when extra precision is required (see ObjectMove) ... for screen-space objects this is called x_pixel instead
x_sub =			 $A ; and $B
y_pos =			 $C ; and $D ... some objects use $E and $F as well when extra precision is required ... screen-space objects use y_pixel instead
y_sub =			 $E ; and $F
priority =		$18 ; 0 = front
width_pixels =		$19
mapping_frame =		$1A
; ---------------------------------------------------------------------------
; conventions followed by most objects:
x_vel =			$10 ; and $11 ; horizontal velocity
y_vel =			$12 ; and $13 ; vertical velocity
y_radius =		$16 ; collision height / 2
x_radius =		$17 ; collision width / 2
anim_frame =		$1B
anim =			$1C
prev_anim =		$1D
anim_frame_duration =	$1E
status =		$22 ; note: exact meaning depends on the object... for Sonic/Tails: bit 0: left-facing. bit 1: in-air. bit 2: spinning. bit 3: on-object. bit 4: roll-jumping. bit 5: pushing. bit 6: underwater.
routine =		$24
routine_secondary =	$25
angle =			$26 ; angle about the z axis (360 degrees = 256)
; ---------------------------------------------------------------------------
; conventions followed by many objects but NOT Sonic/Tails:
collision_flags =	$20
collision_property =	$21
respawn_index =		$23
subtype =		$28
; ---------------------------------------------------------------------------
; conventions specific to Sonic/Tails (Obj01, Obj02, and ObjDB):
; note: $1F, $20, and $21 are unused and available (however, $1F is cleared by loc_A53A and ObjB2_Landed_on_plane)
inertia =		$14 ; and $15 ; directionless representation of speed... not updated in the air
flip_angle =		$27 ; angle about the x axis (360 degrees = 256) (twist/tumble)
air_left =		$28
flip_turned =		$29 ; 0 for normal, 1 to invert flipping (it's a 180 degree rotation about the axis of Sonic's spine, so he stays in the same position but looks turned around)
obj_control =		$2A ; 0 for normal, 1 for hanging or for resting on a flipper, $81 for going through CNZ/OOZ/MTZ tubes or stopped in CNZ cages or stoppers or flying if Tails
status_secondary =	$2B
flips_remaining =	$2C ; number of flip revolutions remaining
flip_speed =		$2D ; number of flip revolutions per frame / 256
move_lock =		$2E ; and $2F ; horizontal control lock, counts down to 0
invulnerable_time =	$30 ; and $31 ; time remaining until you stop blinking
invincibility_time =	$32 ; and $33 ; remaining
speedshoes_time =	$34 ; and $35 ; remaining
next_tilt =		$36 ; angle on ground in front of sprite
tilt =			$37 ; angle on ground
stick_to_convex =	$38 ; 0 for normal, 1 to make Sonic stick to convex surfaces like the rotating discs in Sonic 1 and 3 (unused in Sonic 2 but fully functional)
spindash_flag =		$39 ; 0 for normal, 1 for charging a spindash or forced rolling
pinball_mode =		spindash_flag
spindash_counter =	$3A ; and $3B
restart_countdown =	spindash_counter; and 1+spindash_counter
jumping =		$3C
interact =		$3D ; RAM address of the last object Sonic stood on, minus $FFFFB000 and divided by $40
top_solid_bit = 	$3E ; the bit to check for top solidity (either $C or $E)
lrb_solid_bit =		$3F ; the bit to check for left/right/bottom solidity (either $D or $F)
; ---------------------------------------------------------------------------
; conventions followed by several objects but NOT Sonic/Tails:
y_pixel =		2+x_pos ; and 3+x_pos ; y coordinate for objects using screen-space coordinate system
x_pixel =		x_pos ; and 1+x_pos ; x coordinate for objects using screen-space coordinate system
parent =		objoff_3E ; and $3F ; address of object that owns or spawned this one, if applicable
; TODO: $2C is often parent instead (see LoadChildObject); consider defining parent2 = $2C and changing some objoff_2Cs to that
; ---------------------------------------------------------------------------
; conventions followed by some/most bosses:
boss_subtype		= objoff_A
boss_invulnerable_time	= objoff_14
boss_sine_count		= mapping_frame
boss_routine		= angle
boss_defeated		= objoff_2C
boss_hitcount2		= objoff_32
boss_hurt_sonic		= objoff_38	; flag set by collision response routine when Sonic has just been hurt (by boss?)
; ---------------------------------------------------------------------------
; when childsprites are activated (i.e. bit #6 of render_flags set)
next_subspr		= 6
mainspr_mapframe	= objoff_B
mainspr_width		= objoff_E
mainspr_childsprites 	= objoff_F	; amount of child sprites
mainspr_height		= objoff_14
subspr_data		= $10
sub2_x_pos		= subspr_data+next_subspr*0+0	;x_vel
sub2_y_pos		= subspr_data+next_subspr*0+2	;y_vel
sub2_mapframe		= subspr_data+next_subspr*0+5
sub3_x_pos		= subspr_data+next_subspr*1+0	;y_radius
sub3_y_pos		= subspr_data+next_subspr*1+2	;priority
sub3_mapframe		= subspr_data+next_subspr*1+5	;anim_frame
sub4_x_pos		= subspr_data+next_subspr*2+0	;anim
sub4_y_pos		= subspr_data+next_subspr*2+2	;anim_frame_duration
sub4_mapframe		= subspr_data+next_subspr*2+5	;collision_property
sub5_x_pos		= subspr_data+next_subspr*3+0	;status
sub5_y_pos		= subspr_data+next_subspr*3+2	;routine
sub5_mapframe		= subspr_data+next_subspr*3+5
sub6_x_pos		= subspr_data+next_subspr*4+0	;subtype
sub6_y_pos		= subspr_data+next_subspr*4+2
sub6_mapframe		= subspr_data+next_subspr*4+5
sub7_x_pos		= subspr_data+next_subspr*5+0
sub7_y_pos		= subspr_data+next_subspr*5+2
sub7_mapframe		= subspr_data+next_subspr*5+5
sub8_x_pos		= subspr_data+next_subspr*6+0
sub8_y_pos		= subspr_data+next_subspr*6+2
sub8_mapframe		= subspr_data+next_subspr*6+5
sub9_x_pos		= subspr_data+next_subspr*7+0
sub9_y_pos		= subspr_data+next_subspr*7+2
sub9_mapframe		= subspr_data+next_subspr*7+5
; ---------------------------------------------------------------------------
; unknown or inconsistently used offsets that are not applicable to Sonic/Tails:
; (provided because rearrangement of the above values sometimes requires making space in here too)
objoff_A =		x_sub+0 ; note: x_pos can be 4 bytes, but sometimes the last 2 bytes of x_pos are used for other unrelated things
objoff_B =		x_sub+1 ; unused
objoff_E =		y_sub+0	; unused
objoff_F =		y_sub+1 ; unused
objoff_10 =		x_vel
objoff_14 =		inertia+0
objoff_15 =		inertia+1
objoff_1F =		anim_frame_duration+1
objoff_27 =		$27
objoff_28 =		subtype ; overlaps subtype, but a few objects use it for other things anyway
 enum               objoff_29=$29,objoff_2A=$2A,objoff_2B=$2B,objoff_2C=$2C,objoff_2D=$2D,objoff_2E=$2E,objoff_2F=$2F
 enum objoff_30=$30,objoff_31=$31,objoff_32=$32,objoff_33=$33,objoff_34=$34,objoff_35=$35,objoff_36=$36,objoff_37=$37
 enum objoff_38=$38,objoff_39=$39,objoff_3A=$3A,objoff_3B=$3B,objoff_3C=$3C,objoff_3D=$3D,objoff_3E=$3E,objoff_3F=$3F

; ---------------------------------------------------------------------------
; property of all objects:
object_size_bits =	6
object_size =		1<<object_size_bits ; the size of an object
next_object =		object_size

; ---------------------------------------------------------------------------
; Controller Buttons
;
; Buttons bit numbers
button_up:			EQU	0
button_down:			EQU	1
button_left:			EQU	2
button_right:			EQU	3
button_B:			EQU	4
button_C:			EQU	5
button_A:			EQU	6
button_start:			EQU	7
; Buttons masks (1 << x == pow(2, x))
button_up_mask:			EQU	1<<button_up	; $01
button_down_mask:		EQU	1<<button_down	; $02
button_left_mask:		EQU	1<<button_left	; $04
button_right_mask:		EQU	1<<button_right	; $08
button_B_mask:			EQU	1<<button_B	; $10
button_C_mask:			EQU	1<<button_C	; $20
button_A_mask:			EQU	1<<button_A	; $40
button_start_mask:		EQU	1<<button_start	; $80

; ---------------------------------------------------------------------------
; Constants that can be used instead of hard-coded IDs for various things.
; The "id" function allows to remove elements from an array/table without having
; to change the IDs everywhere in the code.

cur_zone_id := 0 ; the zone ID currently being declared
cur_zone_str := "0" ; string representation of the above

; macro to declare a zone ID
; this macro also declares constants of the form zone_id_X, where X is the ID of the zone in stock Sonic 2
; in order to allow level offset tables to be made dynamic
zoneID macro zoneID,{INTLABEL}
__LABEL__ = zoneID
zone_id_{cur_zone_str} = zoneID
cur_zone_id := cur_zone_id+1
cur_zone_str := "\{cur_zone_id}"
    endm

; Zone IDs. These MUST be declared in the order in which their IDs are in-game, otherwise zone offset tables will screw up
green_hill_zone zoneID		0
ocean_wind_zone zoneID		1	; UNUSED
wood_zone zoneID		2
sand_shower_zone zoneID		3	; UNUSED
metropolis_zone zoneID		4
metropolis_zone_2 zoneID	5
blue_lake_zone zoneID		6	; UNUSED
hill_top_zone zoneID		7
hidden_palace_zone zoneID	8
rock_world_zone zoneID		9	; UNUSED
oil_ocean_zone zoneID		$A
dust_hill_zone zoneID		$B
casino_night_zone zoneID	$C
chemical_plant_zone zoneID	$D
genocide_city_zone zoneID	$E	; EMPTY
neo_green_hill_zone zoneID	$F
death_egg_zone zoneID		$10	; EMPTY, NOT DEFINED IN CERTAIN TABLES

; NOTE: If you want to shift IDs around, set useFullWaterTables to 1 in the assembly options

; set the number of zones
no_of_zones = cur_zone_id

; Zone and act IDs
green_hill_zone_act_1 =		(green_hill_zone<<8)|0
green_hill_zone_act_2 =		(green_hill_zone<<8)|1
ocean_wind_zone_act_1 =		(ocean_wind_zone<<8)|0
ocean_wind_zone_act_2 =		(ocean_wind_zone<<8)|1
wood_zone_act_1 =		(wood_zone<<8)|0
wood_zone_act_2 =		(wood_zone<<8)|1
sand_shower_zone_act_1 =	(sand_shower_zone<<8)|0
sand_shower_zone_act_2 =	(sand_shower_zone<<8)|1
metropolis_zone_act_1 =		(metropolis_zone<<8)|0
metropolis_zone_act_2 =		(metropolis_zone<<8)|1
metropolis_zone_act_3 =		(metropolis_zone_2<<8)|0
metropolis_zone_act_4 =		(metropolis_zone_2<<8)|1
blue_lake_zone_act_1 =		(blue_lake_zone<<8)|0
blue_lake_zone_act_2 =		(blue_lake_zone<<8)|1
hill_top_zone_act_1 =		(hill_top_zone<<8)|0
hill_top_zone_act_2 =		(hill_top_zone<<8)|1
hidden_palace_zone_act_1 =	(hidden_palace_zone<<8)|0
hidden_palace_zone_act_2 =	(hidden_palace_zone<<8)|1
rock_world_zone_act_1 =		(rock_world_zone<<8)|0
rock_world_zone_act_2 =		(rock_world_zone<<8)|1
oil_ocean_zone_act_1 =		(oil_ocean_zone<<8)|0
oil_ocean_zone_act_2 =		(oil_ocean_zone<<8)|1
dust_hill_zone_act_1 =		(dust_hill_zone<<8)|0
dust_hill_zone_act_2 =		(dust_hill_zone<<8)|1
casino_night_zone_act_1 =	(casino_night_zone<<8)|0
casino_night_zone_act_2 =	(casino_night_zone<<8)|1
chemical_plant_zone_act_1 =	(chemical_plant_zone<<8)|0
chemical_plant_zone_act_2 =	(chemical_plant_zone<<8)|1
genocide_city_zone_act_1 =	(genocide_city_zone<<8)|0
genocide_city_zone_act_2 =	(genocide_city_zone<<8)|1
neo_green_hill_zone_act_1 =	(neo_green_hill_zone<<8)|0
neo_green_hill_zone_act_2 =	(neo_green_hill_zone<<8)|1
death_egg_zone_act_1 =		(death_egg_zone<<8)|0
death_egg_zone_act_2 =		(death_egg_zone<<8)|1

; Non-existant/Sonic 1 IDs called
labyrinth_zone_act_4 =		(ocean_wind_zone<<8)|3	; leftover from Sonic 1
scrap_brain_zone_act_2 =	(metropolis_zone_2<<8)|1	; leftover from Sonic 1
metropolis_zone_act_6 =		(metropolis_zone_2<<8)|3	; ??? ; S1 Special Stage code calls this...

; ---------------------------------------------------------------------------
; some variables and functions to help define those constants (redefined before a new set of IDs)
offset :=	0		; this is the start of the pointer table
ptrsize :=	1		; this is the size of a pointer (should be 1 if the ID is a multiple of the actual size)
idstart :=	0		; value to add to all IDs

; function using these variables
id function ptr,((ptr-offset)/ptrsize+idstart)

; V-Int routines
offset :=	Vint_SwitchTbl
ptrsize :=	1
idstart :=	0

VintID_Lag =		id(Vint_Lag_ptr)
VintID_SEGA =		id(Vint_SEGA_ptr)
VintID_Title =		id(Vint_Title_ptr)
VintID_Unused6 =	id(Vint_Unused6_ptr)
VintID_Level =		id(Vint_Level_ptr)
VintID_S1SS =		id(Vint_S1SS_ptr)
VintID_TitleCard =	id(Vint_TitleCard_ptr)
VintID_UnusedE =	id(Vint_UnusedE_ptr)
VintID_Pause =		id(Vint_Pause_ptr)
VintID_Fade =		id(Vint_Fade_ptr)
VintID_PCM =		id(Vint_PCM_ptr)
VintID_SSResults =	id(Vint_SSResults_ptr)
VintID_TitleCardDup =	id(Vint_TitleCardDup_ptr)

; Game modes
offset :=	GameModesArray
ptrsize :=	1
idstart :=	0

GameModeID_SegaScreen =		id(GameMode_SegaScreen)
GameModeID_TitleScreen =	id(GameMode_TitleScreen)
GameModeID_Demo =		id(GameMode_Demo)
GameModeID_Level =		id(GameMode_Level)
GameModeID_SpecialStage =	id(GameMode_SpecialStage)
GameModeFlag_TitleCard =	7 ; flag bit
GameModeID_TitleCard =		1<<GameModeFlag_TitleCard ; flag mask

S1GameModeID_ContinueScreen =	$14
S1GameModeID_Credits =		$1C

; palette IDs
offset :=	PalPointers
ptrsize :=	8
idstart :=	0

PalID_SEGA =		id(PalPtr_SEGA)
PalID_Title =		id(PalPtr_Title)
PalID_LevelSel =	id(PalPtr_LevelSel)
PalID_SonicTails =	id(PalPtr_SonicTails)
PalID_GHZ =		id(PalPtr_GHZ)
PalID_OWZ =		id(PalPtr_OWZ)
PalID_WZ =		id(PalPtr_WZ)
PalID_SSZ =		id(PalPtr_SSZ)
PalID_MTZ =		id(PalPtr_MTZ)
PalID_MTZ2 =		id(PalPtr_MTZ2)
PalID_BLZ =		id(PalPtr_BLZ)
PalID_HTZ =		id(PalPtr_HTZ)
PalID_HPZ =		id(PalPtr_HPZ)
PalID_RWZ =		id(PalPtr_RWZ)
PalID_OOZ =		id(PalPtr_OOZ)
PalID_DHZ =		id(PalPtr_DHZ)
PalID_CNZ =		id(PalPtr_CNZ)
PalID_CPZ =		id(PalPtr_CPZ)
PalID_GCZ =		id(PalPtr_GCZ)
PalID_NGHZ =		id(PalPtr_NGHZ)
PalID_DEZ =		id(PalPtr_DEZ)
PalID_HPZ_U =		id(PalPtr_HPZ_U)
PalID_CPZ_U =		id(PalPtr_CPZ_U)
PalID_NGHZ_U =		id(PalPtr_NGHZ_U)
PalID_SpecStg =		id(PalPtr_SpecStg)

S1PalID_SpecStg =	PalID_CPZ_U	; leftover from Sonic 1 Special Stage
PalID_CNZ2 =		PalID_BLZ	; loaded in CNZ2; identical to CNZ1 palette

; PLC IDs
offset :=	ArtLoadCues
ptrsize :=	2
idstart :=	0

PLCID_Std1 =		id(PLCptr_Std1)
PLCID_Std2 =		id(PLCptr_Std2)
PLCID_StdExp =		id(PLCptr_StdExp)
PLCID_GameOver =	id(PLCptr_GameOver)
PLCID_Ghz1 =		id(PLCptr_Ghz1)
PLCID_Ghz2 =		id(PLCptr_Ghz2)
PLCID_Owz1 =		id(PLCptr_Owz1)
PLCID_Owz2 =		id(PLCptr_Owz2)
PLCID_Wz1 =		id(PLCptr_Wz1)
PLCID_Wz2 =		id(PLCptr_Wz2)
PLCID_Ssz1 =		id(PLCptr_Ssz1)
PLCID_Ssz2 =		id(PLCptr_Ssz2)
PLCID_Mtz1 =		id(PLCptr_Mtz1)
PLCID_Mtz2 =		id(PLCptr_Mtz2)
PLCID_Mtz3 =		id(PLCptr_Mtz3)
PLCID_Mtz4 =		id(PLCptr_Mtz4)
PLCID_Blz1 =		id(PLCptr_Blz1)
PLCID_Blz2 =		id(PLCptr_Blz2)
PLCID_Htz1 =		id(PLCptr_Htz1)
PLCID_Htz2 =		id(PLCptr_Htz2)
PLCID_Hpz1 =		id(PLCptr_Hpz1)
PLCID_Hpz2 =		id(PLCptr_Hpz2)
PLCID_Rwz1 =		id(PLCptr_Rwz1)
PLCID_Rwz2 =		id(PLCptr_Rwz2)
PLCID_Ooz1 =		id(PLCptr_Ooz1)
PLCID_Ooz2 =		id(PLCptr_Ooz2)
PLCID_Dhz1 =		id(PLCptr_Dhz1)
PLCID_Dhz2 =		id(PLCptr_Dhz2)
PLCID_Cnz1 =		id(PLCptr_Cnz1)
PLCID_Cnz2 =		id(PLCptr_Cnz2)
PLCID_Cpz1 =		id(PLCptr_Cpz1)
PLCID_Cpz2 =		id(PLCptr_Cpz2)
PLCID_Gcz1 =		id(PLCptr_Gcz1)
PLCID_Gcz2 =		id(PLCptr_Gcz2)
PLCID_Nghz1 =		id(PLCptr_Nghz1)
PLCID_Nghz2 =		id(PLCptr_Nghz2)
PLCID_Dez1 =		id(PLCptr_Dez1)
PLCID_Dez2 =		id(PLCptr_Dez2)
PLCID_Results =		id(PLCptr_Results)
PLCID_Signpost =	id(PLCptr_Signpost)
PLCID_GhzBoss =		id(PLCptr_GhzBoss)

S1PLCID_SpecStg = PLCID_Hpz1
S1PLCID_SpecRes = PLCID_Dhz2

; Music IDs
offset :=	zMasterPlaylist
ptrsize :=	1
idstart :=	$81
; $80 is reserved for silence, so if you make idstart $80 or less,
; you may need to insert a dummy zMusIDPtr in the $80 slot

MusID__First = idstart
MusID_OOZ =		id(zMusIDPtr_OOZ)
MusID_GHZ =		id(zMusIDPtr_GHZ)
MusID_MTZ =		id(zMusIDPtr_MTZ)
MusID_CNZ =		id(zMusIDPtr_CNZ)
MusID_DHZ =		id(zMusIDPtr_DHZ)
MusID_HPZ =		id(zMusIDPtr_HPZ)
MusID_NGHZ =		id(zMusIDPtr_NGHZ)
MusID_DEZ =		id(zMusIDPtr_DEZ)
MusID_SpecStg =		id(zMusIDPtr_SpecStg)
MusID_LevelSel =	id(zMusIDPtr_LevelSel)		; according to the code that handles drowning, this was where the developers were planning to put the drowning theme
MusID_LevelSelDup =	id(zMusIDPtr_LevelSelDup)
MusID_FinalBoss =	id(zMusIDPtr_FinalBoss)
MusID_CPZ =		id(zMusIDPtr_CPZ)
MusID_Boss =		id(zMusIDPtr_Boss)
MusID_RWZ =		id(zMusIDPtr_RWZ)
MusID_SSZ =		id(zMusIDPtr_SSZ)
MusID_SSZDup =		id(zMusIDPtr_SSZDup)
MusID_Unused1 =		id(zMusIDPtr_Unused1)
MusID_BOZ =		id(zMusIDPtr_BOZ)
MusID_Unused2 =		id(zMusIDPtr_Unused2)
MusID_Invinc =		id(zMusIDPtr_Invinc)
MusID_HTZ =		id(zMusIDPtr_HTZ)
MusID_HTZDup =		id(zMusIDPtr_HTZDup)
MusID_ExtraLife =	id(zMusIDPtr_ExtraLife)
MusID_Title =		id(zMusIDPtr_Title)
MusID_ActClear =	id(zMusIDPtr_ActClear)
MusID_GameOver =	id(zMusIDPtr_GameOver)
MusID_Continue =	id(zMusIDPtr_Continue)
MusID_Emerald =		id(zMusIDPtr_Emerald)
MusID_EmeraldDup =	id(zMusIDPtr_EmeraldDup)
MusID_EmeraldDup2 =	id(zMusIDPtr_EmeraldDup2)
MusID__End =		id(zMusIDPtr__End)

; Whenever the music references a slot that was its placement in Sonic 1
S1MusID_LZ =		$82
S1MusID_Invinc =	$87
S1MusID_ExtraLife =	$88
S1MusID_Boss =		$8C
S1MusID_ActClear =	$8E
S1MusID_Emerald =	$93
S1SndID_Waterfall =	$D0
S1MusID_Stop =		$E0

; Sound IDs
offset :=	SoundIndex
ptrsize :=	2
idstart :=	$A0
; $80 is reserved for silence, so if you make idstart $80 or less,
; you may need to insert a dummy SndPtr in the $80 slot

SndID__First = idstart
SndID_Jump =		id(SndPtr_Jump)			; A0
SndID_Checkpoint =	id(SndPtr_Checkpoint)		; A1
SndID_SpikeSwitch =	id(SndPtr_SpikeSwitch)		; A2
SndID_Hurt =		id(SndPtr_Hurt)			; A3
SndID_Skidding =	id(SndPtr_Skidding)		; A4
SndID_MissileDissolve =	id(SndPtr_MissileDissolve)	; A5
SndID_HurtBySpikes =	id(SndPtr_HurtBySpikes)		; A6
SndID_PushBlock =	id(SndPtr_PushBlock)		; A7
SndID_SSGoal =		id(SndPtr_SSGoal)		; A8
SndID_Bwoop =		id(SndPtr_Bwoop)		; A9
SndID_Splash =		id(SndPtr_Splash)		; AA
SndID_Swish =		id(SndPtr_Swish)		; AB
SndID_BossHit =		id(SndPtr_BossHit)		; AC
SndID_InhalingBubble =	id(SndPtr_InhalingBubble)	; AD
SndID_ArrowFiring =	id(SndPtr_ArrowFiring)		; AE
SndID_LavaBall =	id(SndPtr_LavaBall)		; AE
SndID_Shield =		id(SndPtr_Shield)		; AF
SndID_Saw =		id(SndPtr_Saw)			; B0
SndID_Electric =	id(SndPtr_Electric)		; B1
SndID_Drown =		id(SndPtr_Drown)		; B2
SndID_FireBurn =	id(SndPtr_FireBurn)		; B3
SndID_Bumper =		id(SndPtr_Bumper)		; B4
SndID_Ring =		id(SndPtr_Ring)			; B5
SndID_RingRight =	id(SndPtr_RingRight)		; B5
SndID_SpikesMove =	id(SndPtr_SpikesMove)		; B6
SndID_Rumbling =	id(SndPtr_Rumbling)		; B7
SndID_Smash =		id(SndPtr_Smash)		; B9
SndID_SSGlass =		id(SndPtr_SSGlass)		; BA
SndID_DoorSlam =	id(SndPtr_DoorSlam)		; BB
SndID_SpindashRelease =	id(SndPtr_SpindashRelease)	; BC
SndID_Hammer =		id(SndPtr_Hammer)		; BD
SndID_Roll =		id(SndPtr_Roll)			; BE
SndID_ContinueJingle =	id(SndPtr_ContinueJingle)	; BF
SndID_BasaranFlap =	id(SndPtr_BasaranFlap)		; C0
SndID_Explosion =	id(SndPtr_Explosion)		; C1
SndID_WaterWarning =	id(SndPtr_WaterWarning)		; C2
SndID_EnterGiantRing =	id(SndPtr_EnterGiantRing)	; C3
SndID_BossExplosion =	id(SndPtr_BossExplosion)	; C4
SndID_TallyEnd =	id(SndPtr_TallyEnd)		; C5
SndID_RingSpill =	id(SndPtr_RingSpill)		; C6
SndID_Flamethrower =	id(SndPtr_Flamethrower)		; C8
SndID_Bonus =		id(SndPtr_Bonus)		; C9
SndID_SpecStageEntry =	id(SndPtr_SpecStageEntry)	; CA
SndID_SlowSmash =	id(SndPtr_SlowSmash)		; CB
SndID_Spring =		id(SndPtr_Spring)		; CC
SndID_Blip =		id(SndPtr_Blip)			; CD
SndID_RingLeft =	id(SndPtr_RingLeft)		; CE
SndID_Signpost =	id(SndPtr_Signpost)		; CF
SndID_CNZBossZap =	id(SndPtr_CNZBossZap)		; D0
SndID_Signpost2P =	id(SndPtr_Signpost2P)		; D3
SndID_OOZLidPop =	id(SndPtr_OOZLidPop)		; D4
SndID_SlidingSpike =	id(SndPtr_SlidingSpike)		; D5
SndID_CNZElevator =	id(SndPtr_CNZElevator)		; D6
SndID_PlatformKnock =	id(SndPtr_PlatformKnock)	; D7
SndID_BonusBumper =	id(SndPtr_BonusBumper)		; D8
SndID_LargeBumper =	id(SndPtr_LargeBumper)		; D9
SndID_Gloop =		id(SndPtr_Gloop)		; DA
SndID_PreArrowFiring =	id(SndPtr_PreArrowFiring)	; DB
SndID_Fire =		id(SndPtr_Fire)			; DC
SndID_ArrowStick =	id(SndPtr_ArrowStick)		; DD
SndID_Helicopter =	id(SndPtr_Helicopter)		; DE
SndID_SuperTransform =	id(SndPtr_SuperTransform)	; DF
SndID_SpindashRev =	id(SndPtr_SpindashRev)		; E0
SndID__End =		id(SndPtr__End)			; E1

; Sound command IDs
offset :=	zCommandIndex
ptrsize :=	4
idstart :=	$F9

CmdID__First = idstart
MusID_FadeOut =		id(CmdPtr_FadeOut)	; F9
SndID_SegaSound =	id(CmdPtr_SegaSound)	; FA
MusID_SpeedUp =		id(CmdPtr_SpeedUp)	; FB
MusID_SlowDown =	id(CmdPtr_SlowDown)	; FC
MusID_Stop =		id(CmdPtr_Stop)		; FD
CmdID__End =		id(CmdPtr__End)		; FE

MusID_Pause =		$7E+$80			; FE
MusID_Unpause =		$7F+$80			; FF

; Other sizes
palette_line_size =	$10*2	; 16 word entries

; ---------------------------------------------------------------------------
; I run the main 68k RAM addresses through this function
; to let them work in both 16-bit and 32-bit addressing modes.
ramaddr function x,-(-x)&$FFFFFFFF

; ---------------------------------------------------------------------------
; RAM variables - General
	phase	ramaddr($FFFF0000)	; Pretend we're in the RAM
RAM_Start:
Chunk_Table:			ds.b	$8000
Chunk_Table_End:
Level_Layout:			ds.b	$1000
Level_Layout_End:

Block_Table:			ds.b	$1800
Block_Table_End:

TempArray_LayerDef:		ds.b	$200	; used by some layer deformation routines
Decomp_Buffer:			ds.b	$200
Decomp_Buffer_End:
Sprite_Table_Input:		ds.b	$400	; in custom format before being converted and stored in Sprite_Table/Sprite_Table_2
Sprite_Table_Input_End:

; haven't gotten to documenting this yet, but this is here for clearRAM
Object_RAM:			; The various objects in the game are loaded in this area.
				; Each game mode uses different objects, so some slots are reused.
				; The section below declares labels for all objects, since there's really only three screens at this point
Reserved_Object_RAM:
MainCharacter:			; first object (usually Sonic)
				ds.b	object_size
Sidekick:			; second object (usually Tails)
TitleScreen_Sonic:		; Sonic from the title screen
				ds.b	object_size
TitleCard:
TitleCard_ZoneName:		; level title card: zone name
TitleScreen_Tails:		; Tails from the title screen
GameOver_GameText:		; "GAME" from GAME OVER
TimeOver_TimeText:		; "TIME" from TIME OVER
				ds.b	object_size
TitleCard_Zone:			; level title card: "ZONE"
GameOver_OverText:		; "OVER" from GAME OVER
TimeOver_OverText:		; "OVER" from TIME OVER
				ds.b	object_size
TitleCard_ActNumber:		; level title card: act number
				ds.b	object_size
TitleCard_Background:		; level title card: background
				ds.b	object_size
Shield:
				ds.b	object_size
Tails_Tails:			; address of the Tail's Tails object
				ds.b	object_size
InvincibilityStars:
				ds.b	object_size
				; Reserved object RAM: free slots
				ds.b	object_size
				ds.b	object_size
				ds.b	object_size
WaterSplash:			; Sonic's water splash
				ds.b	object_size
BreathingBubbles:		; Sonic's breathing bubbles
				ds.b	object_size
HeadsUpDisplay:			; HUD (still uses Sonic 1's HUD system at this point)
				ds.b	object_size
				ds.b	$1C40 ; RESERVED FOR OBJECT RAM, DO NOT REMOVE!!!
Object_RAM_End:

Primary_Collision:		ds.b	$600
Secondary_Collision:		ds.b	$600

VDP_Command_Buffer:		ds.w	7*$12	; stores 18 ($12) VDP commands to issue the next time ProcessDMAQueue is called
VDP_Command_Buffer_Slot:	ds.l	1	; stores the address of the next open slot for a queued VDP command

Sprite_Table_2:			ds.b	$280	; Sprite attribute table buffer for the bottom split screen in 2-player mode
				ds.b	$80	; unused, but SAT buffer can spill over into this area when there are too many sprites on-screen

Horiz_Scroll_Buf:		ds.l	224
Horiz_Scroll_Buf_End:
				ds.l	16 	; A bug/optimisation in 'SwScrl_CPZ' causes 'Horiz_Scroll_Buf' to overflow into this.
				ds.b	$40	; unused
Horiz_Scroll_Buf_End_Padded:

Sonic_Stat_Record_Buf:		ds.b	$100

Sonic_Pos_Record_Buf:		ds.b	$100
Sonic_Pos_Record_Buf_End:

unk_E600:			ds.b	$100

Tails_Pos_Record_Buf:		ds.b	$100
Tails_Pos_Record_Buf_End:

Ring_Positions:			ds.b	$600
Ring_Positions_End:

Camera_RAM:

Camera_Positions:
Camera_X_pos:			ds.l	1
Camera_Y_pos:			ds.l	1
Camera_BG_X_pos:		ds.l	1	; only used sometimes as the layer deformation makes it sort of redundant
Camera_BG_Y_pos:		ds.l	1
Camera_BG2_X_pos:		ds.l	1	; used in CPZ
Camera_BG2_Y_pos:		ds.l	1	; used in CPZ
Camera_BG3_X_pos:		ds.l	1	; unused (only initialised at beginning of level)?
Camera_BG3_Y_pos:		ds.l	1	; unused (only initialised at beginning of level)?
Camera_Positions_End:

Camera_Positions_P2:
Camera_X_pos_P2:		ds.l	1
Camera_Y_pos_P2:		ds.l	1
Camera_BG_X_pos_P2:		ds.l	1	; only used sometimes as the layer deformation makes it sort of redundant
Camera_BG_Y_pos_P2:		ds.l	1
Camera_BG2_X_pos_P2:		ds.l	1	; unused (only initialised at beginning of level)?
Camera_BG2_Y_pos_P2:		ds.l	1
Camera_BG3_X_pos_P2:		ds.l	1	; unused (only initialised at beginning of level)?
Camera_BG3_Y_pos_P2:		ds.l	1
Camera_Positions_P2_End:

Block_Crossed_Flags:
Horiz_block_crossed_flag:	ds.b	1	; toggles between 0 and $10 when you cross a block boundary horizontally
Verti_block_crossed_flag:	ds.b	1	; toggles between 0 and $10 when you cross a block boundary vertically
Horiz_block_crossed_flag_BG:	ds.b	1	; toggles between 0 and $10 when background camera crosses a block boundary horizontally
Verti_block_crossed_flag_BG:	ds.b	1	; toggles between 0 and $10 when background camera crosses a block boundary vertically
Horiz_block_crossed_flag_BG2:	ds.b	1	; used in CPZ
				ds.b	1	; $FFFFEE45 ; seems unused
Horiz_block_crossed_flag_BG3:	ds.b	1
				ds.b	1	; $FFFFEE47 ; seems unused
Block_Crossed_Flags_End:

Block_Crossed_Flags_P2:
Horiz_block_crossed_flag_P2:	ds.b	1	; toggles between 0 and $10 when you cross a block boundary horizontally
Verti_block_crossed_flag_P2:	ds.b	1	; toggles between 0 and $10 when you cross a block boundary vertically
				ds.b	6	; $FFFFEE4A-$FFFFEE4F ; seems unused
Block_Crossed_Flags_P2_End:

Scroll_Flags_All:
Scroll_flags:			ds.w	1	; bitfield ; bit 0 = redraw top row, bit 1 = redraw bottom row, bit 2 = redraw left-most column, bit 3 = redraw right-most column
Scroll_flags_BG:		ds.w	1	; bitfield ; bits 0-3 as above, bit 4 = redraw top row (except leftmost block), bit 5 = redraw bottom row (except leftmost block), bits 6-7 = as bits 0-1
Scroll_flags_BG2:		ds.w	1	; bitfield ; essentially unused; bit 0 = redraw left-most column, bit 1 = redraw right-most column
Scroll_flags_BG3:		ds.w	1	; bitfield ; for CPZ; bits 0-3 as Scroll_flags_BG but using Y-dependent BG camera; bits 4-5 = bits 2-3; bits 6-7 = bits 2-3
Scroll_Flags_All_End:

Scroll_Flags_All_P2:
Scroll_flags_P2:		ds.w	1	; bitfield ; bit 0 = redraw top row, bit 1 = redraw bottom row, bit 2 = redraw left-most column, bit 3 = redraw right-most column
Scroll_flags_BG_P2:		ds.w	1	; bitfield ; bits 0-3 as above, bit 4 = redraw top row (except leftmost block), bit 5 = redraw bottom row (except leftmost block), bits 6-7 = as bits 0-1
Scroll_flags_BG2_P2:		ds.w	1	; bitfield ; essentially unused; bit 0 = redraw left-most column, bit 1 = redraw right-most column
Scroll_flags_BG3_P2:		ds.w	1	; bitfield ; for CPZ; bits 0-3 as Scroll_flags_BG but using Y-dependent BG camera; bits 4-5 = bits 2-3; bits 6-7 = bits 2-3
Scroll_Flags_All_P2_End:

Camera_Positions_Copy:
Camera_RAM_copy:		ds.l	2	; copied over every V-int
Camera_BG_copy:			ds.l	2	; copied over every V-int
Camera_BG2_copy:		ds.l	2	; copied over every V-int
Camera_BG3_copy:		ds.l	2	; copied over every V-int
Camera_Positions_Copy_End:

Camera_Positions_Copy_P2:
Camera_P2_copy:			ds.l	8	; copied over every V-int
Camera_Positions_Copy_P2_End:

Scroll_Flags_Copy_All:
Scroll_flags_copy:		ds.w	1	; copied over every V-int
Scroll_flags_BG_copy:		ds.w	1	; copied over every V-int
Scroll_flags_BG2_copy:		ds.w	1	; copied over every V-int
Scroll_flags_BG3_copy:		ds.w	1	; copied over every V-int
Scroll_Flags_Copy_All_End:

Scroll_Flags_Copy_All_P2:
Scroll_flags_copy_P2:		ds.w	1	; copied over every V-int
Scroll_flags_BG_copy_P2:	ds.w	1	; copied over every V-int
Scroll_flags_BG2_copy_P2:	ds.w	1	; copied over every V-int
Scroll_flags_BG3_copy_P2:	ds.w	1	; copied over every V-int
Scroll_Flags_Copy_All_P2_End:

Camera_Difference:
Camera_X_pos_diff:		ds.w	1	; (new X pos - old X pos) * 256
Camera_Y_pos_diff:		ds.w	1	; (new Y pos - old Y pos) * 256
Camera_Difference_End:

Camera_BG_X_pos_diff:		ds.w	1	; Effective camera change used in HTZ screen shake
Camera_BG_Y_pos_diff:		ds.w	1	; Effective camera change used in HTZ screen shake

Camera_Difference_P2:
Camera_X_pos_diff_P2:		ds.w	1	; (new X pos - old X pos) * 256
Camera_Y_pos_diff_P2:		ds.w	1	; (new Y pos - old Y pos) * 256
Camera_Difference_P2_End:

Screen_Shaking_Flag_HTZ:	ds.b	1	; activates screen shaking code in HTZ's layer deformation routine
Screen_Shaking_Flag:		ds.b	1	; activates screen shaking code (if existent) in layer deformation routine
				ds.b	2	; $FFFFEEBE-$FFFFEEBF ; unused
unk_EEC0:			ds.l	1	; unused, except on write in LevelSizeLoad...
unk_EEC4:			ds.w	1	; same as above. The write being a long also overwrites the address below
Camera_Max_Y_pos:		ds.w	1

Camera_Boundaries:
Camera_Min_X_pos:		ds.w	1
Camera_Max_X_pos:		ds.w	1
Camera_Min_Y_pos:		ds.w	1
Camera_Max_Y_pos_now:		ds.w	1
Camera_Boundaries_End:

Camera_Delay:
Horiz_scroll_delay_val:		ds.w	1	; if its value is a, where a != 0, X scrolling will be based on the player's X position a-1 frames ago
Sonic_Pos_Record_Index:		ds.w	1	; into Sonic_Pos_Record_Buf and Sonic_Stat_Record_Buf
Camera_Delay_End:

Camera_Delay_P2:
Horiz_scroll_delay_val_P2:	ds.w	1
Tails_Pos_Record_Index:		ds.w	1	; into Tails_Pos_Record_Buf
Camera_Delay_P2_End:

Camera_Y_pos_bias:		ds.w	1	; added to y position for lookup/lookdown, $60 is center
Camera_Y_pos_bias_End:

				ds.b	2	; $FFFFEEDA ; seems unused
Deform_lock:			ds.b	1	; set to 1 to stop all deformation
				ds.b	1	; $FFFFEEDD ; seems unused
Camera_Max_Y_Pos_Changing:	ds.b	1
Dynamic_Resize_Routine:		ds.b	1
unk_EEE0:			ds.w	1	; used in Unused_RecordPos and Tails' AI code
Camera_BG_X_offset:		ds.w	1	; Used to control background scrolling in X in WFZ ending and HTZ screen shake
Camera_BG_Y_offset:		ds.w	1	; Used to control background scrolling in Y in WFZ ending and HTZ screen shake
HTZ_Terrain_Delay:		ds.w	1	; During HTZ screen shake, this is a delay between rising and sinking terrain during which there is no shaking
HTZ_Terrain_Direction:		ds.b	1	; During HTZ screen shake, 0 if terrain/lava is rising, 1 if lowering
				ds.b	3	; $FFFFEEE9-$FFFFEEEB ; seems unused
Vscroll_Factor_P2_HInt:		ds.l	1
Camera_X_pos_copy:		ds.l	1
Camera_Y_pos_copy:		ds.l	1
				ds.b	8	; $FFFFEEF8-$FFFFEEFF ; seems unused

Camera_RAM_End:

Block_cache:			ds.w	512/16*2	; Width of plane in blocks, with each block getting two words.
Ring_consumption_table:		ds.b	$80	; contains RAM addresses of rings currently being consumed
Ring_consumption_table_End:

				ds.b	$600	; $FFFFF000-$FFFFF5FF ; unused, leftover from the Sonic 1 sound driver

Game_Mode:			ds.b	1	; see GameModesArray (master level trigger, Mstr_Lvl_Trigger)
				ds.b	1	; unused
Ctrl_1_Logical:					; 2 bytes
Ctrl_1_Held_Logical:		ds.b	1	; 1 byte
Ctrl_1_Press_Logical:		ds.b	1	; 1 byte
Ctrl_1:						; 2 bytes
Ctrl_1_Held:			ds.b	1	; 1 byte ; (pressed and held were switched around before)
Ctrl_1_Press:			ds.b	1	; 1 byte
Ctrl_2:						; 2 bytes
Ctrl_2_Held:			ds.b	1	; 1 byte
Ctrl_2_Press:			ds.b	1	; 1 byte
				ds.b	4	; $FFFFF608-$FFFFF60B ; seems unused
VDP_Reg1_val:			ds.w	1	; normal value of VDP register #1 when display is disabled
				ds.b	6	; $FFFFF60E-$FFFFF613 ; seems unused
Demo_Time_left:			ds.w	1	; 2 bytes

Vscroll_Factor:
Vscroll_Factor_FG:		ds.w	1
Vscroll_Factor_BG:		ds.w	1
unk_F61A:			ds.l	1	; Only ever cleared, never used
Vscroll_Factor_P2:
Vscroll_Factor_P2_FG:		ds.w	1
Vscroll_Factor_P2_BG:		ds.w	1
				ds.b	2	; $FFFFF622-$FFFFF623 ; seems unused
Hint_counter_reserve:		ds.w	1	; Must contain a VDP command word, preferably a write to register $0A. Executed every V-INT.
Palette_fade_range:				; Range affected by the palette fading routines
Palette_fade_start:		ds.b	1	; Offset from the start of the palette to tell what range of the palette will be affected in the palette fading routines
Palette_fade_length:		ds.b	1	; Number of entries to change in the palette fading routines

MiscLevelVariables:
VIntSubE_RunCount:		ds.b	1
				ds.b	1	; $FFFFF629 ; seems unused
Vint_routine:			ds.b	1	; routine counter for V-int
				ds.b	1	; $FFFFF62B ; seems unused
Sprite_count:			ds.b	1	; the number of sprites drawn in the current frame
				ds.b	5	; $FFFFF62D-$FFFFF631 ; seems unused
PalCycle_Frame:			ds.w	1	; ColorID loaded in PalCycle
PalCycle_Timer:			ds.w	1	; number of frames until next PalCycle call
RNG_seed:			ds.l	1	; used for random number generation
Game_paused:			ds.w	1	
				ds.b	4	; $FFFFF63C-$FFFFF63F ; seems unused
DMA_data_thunk:			ds.w	1	; Used as a RAM holder for the final DMA command word. Data will NOT be preserved across V-INTs, so consider this space reserved.
				ds.w	1	; $FFFFF642-$FFFFF643 ; seems unused
Hint_flag:			ds.w	1	; unless this is 1, H-int won't run

Water_Level_1:			ds.w	1
Water_Level_2:			ds.w	1
Water_Level_3:			ds.w	1
Water_on:			ds.b	1	; is set based on Water_flag
Water_routine:			ds.b	1
Water_fullscreen_flag:		ds.b	1	; was "Water_move"
Do_Updates_in_H_int:		ds.b	1

				ds.b	2	; $FFFFF650-$FFFFF651 ; seems unused
PalCycle_Frame2:		ds.w	1
PalCycle_Frame3:		ds.w	1
				ds.b	6	; $FFFFF656-$FFFFF65B ; seems unused
Palette_frame:			ds.w	1
Palette_timer:			ds.b	1
Super_Sonic_palette:		ds.b	1

unk_F660:			ds.w	1	; Cleared once, never used
unk_F662:			ds.w	1	; Cleared once, never used
				ds.b	2	; $FFFFF664-$FFFFF665 ; seems unused
PalCycle_Timer2:		ds.w	1
PalCycle_Timer3:		ds.w	1
				ds.b	$16	; $FFFFF66A-$FFFFF67F ; seems unused
MiscLevelVariables_End

Plc_Buffer:			ds.b	$60	; Pattern load queue (each entry is 6 bytes)
Plc_Buffer_Only_End:
				; these seem to store nemesis decompression state so PLC processing can be spread out across frames
Plc_Buffer_Reg0:		ds.l	1	
Plc_Buffer_Reg4:		ds.l	1	
Plc_Buffer_Reg8:		ds.l	1	
Plc_Buffer_RegC:		ds.l	1	
Plc_Buffer_Reg10:		ds.l	1	
Plc_Buffer_Reg14:		ds.l	1	
Plc_Buffer_Reg18:		ds.w	1	; amount of current entry remaining to decompress
Plc_Buffer_Reg1A:		ds.w	1	
				ds.b	4	; seems unused
Plc_Buffer_End:

Misc_Variables:
unk_F700:			ds.w	1	; cleared once in Tails CPU routine, never used

; extra variables for the second player (CPU) in 1-player mode
Tails_control_counter:		ds.w	1	; how long until the CPU takes control
				ds.b	2	; $FFFFF704-$FFFFF705 ; seems unused
unk_F706:			ds.w	1	; used in unused Tails CPU routines
Tails_CPU_routine:		ds.w	1
				ds.b	6	; $FFFFF70A-$FFFFF70F ; seems unused

Rings_manager_routine:		ds.b	1
Level_started_flag:		ds.b	1

Ring_Manager_Addresses:
Ring_start_addr:		ds.w	1
Ring_end_addr:			ds.w	1
Ring_Manager_Addresses_End:

Ring_Manager_Addresses_P2:
Ring_start_addr_P2:		ds.w	1
Ring_end_addr_P2:		ds.w	1
Ring_Manager_Addresses_P2_End:
				ds.b	6	; $FFFFF71A-$FFFFF71F ; seems unused

Screen_redraw_flag:		ds.b	1	; if whole screen needs to redraw
CPZ_UnkScroll_Timer:		ds.b	1	; used only in unused CPZ scrolling function
				ds.b	$E	; $FFFFF722-$FFFFF72F ; seems unused
Water_flag:			ds.b	1	; if the level has water or oil
				ds.b	$F	; $FFFFF731-$FFFFF73F ; seems unused
Demo_button_index_2P:		ds.w	1	; index into button press demo data, for player 2
Demo_press_counter_2P:		ds.w	1	; frames remaining until next button press, for player 2
				ds.b	$C	; $FFFFF744-$FFFFF74F ; seems unused

SpecialStage_angle:		ds.w	1	; current angle of Special Stage
SpecialStage_speed:		ds.b	1	; switches between slow or fast depending on whether the UP/DOWN blocks are hit (also written to next byte)
SpecialStage_direction:		ds.b	1	; current turning direction
				ds.b	$C	; $FFFFF754-$FFFFF75F ; seems unused

Sonic_Speeds:
Sonic_top_speed:		ds.w	1
Sonic_acceleration:		ds.w	1
Sonic_deceleration:		ds.w	1
Sonic_Speeds_End:

Sonic_LastLoadedDPLC:		ds.b	1	; mapping frame number when Sonic last had his tiles requested to be transferred from ROM to VRAM. can be set to a dummy value like -1 to force a refresh DMA
				ds.b	1	; $FFFFF767 ; seems unused
Primary_Angle:			ds.b	1
				ds.b	1	; $FFFFF769 ; seems unused
Secondary_Angle:		ds.b	1
				ds.b	1	; $FFFFF76B ; seems unused
Obj_placement_routine:		ds.b	1
				ds.b	1	; $FFFFF76D ; seems unused
Camera_X_pos_last:		ds.w	1	; Camera_X_pos_coarse from the previous frame
Camera_X_pos_last_End:

Object_Manager_Addresses:
Obj_load_addr_right:		ds.l	1	; contains the address of the next object to load when moving right
Obj_load_addr_left:		ds.l	1	; contains the address of the last object loaded when moving left
Object_Manager_Addresses_End:

Object_Manager_Addresses_P2:
Obj_load_addr_right_P2:		ds.l	1
Obj_load_addr_left_P2:		ds.l	1
Object_Manager_Addresses_P2_End:

Object_manager_2P_RAM:	; The next 16 bytes belong to this.
Object_RAM_block_indices:	ds.b	6	; seems to be an array of horizontal chunk positions, used for object position range checks
Player_1_loaded_object_blocks:	ds.b	3
Player_2_loaded_object_blocks:	ds.b	3

Camera_X_pos_last_P2:		ds.w	1
Camera_X_pos_last_P2_End:

Obj_respawn_index_P2:		ds.b	2	; respawn table indices of the next objects when moving left or right for the second player
Obj_respawn_index_P2_End:
Object_manager_2P_RAM_End:

Demo_button_index:		ds.w	1	; index into button press demo data, for player 1
Demo_press_counter:		ds.b	1	; frames remaining until next button press, for player 1
				ds.b	1	; $FFFFF793 ; seems unused
PalChangeSpeed:			ds.w	1
Collision_addr:			ds.l	1

SSPalCycle_Frame:		ds.w	1
SSPalCycle_Timer:		ds.w	1
unk_F79E:			ds.w	1
unk_F7A0:			ds.w	1
				ds.b	5	; $FFFFF7A2-$FFFFF7A6 ; seems unused
Boss_defeated_flag:		ds.b	1
				ds.b	2	; $FFFFF7A8-$FFFFF7A9 ; seems unused
Current_Boss_ID:		ds.b	1
				ds.b	5	; $FFFFF7AB-$FFFFF7AF ; seems unused
MTZ_Platform_Cog_X:		ds.w	1	; X position of moving MTZ platform for cog animation.
MTZCylinder_Angle_Sonic:	ds.b	1
MTZCylinder_Angle_Tails:	ds.b	1
				ds.b	$A	; $FFFFF7B4-$FFFFF7BD ; seems unused
BigRingGraphics:		ds.w	1	; S1 holdover
				ds.b	7	; $FFFFF7C0-$FFFFF7C6 ; seems unused
WindTunnel_flag:		ds.b	1
				ds.b	1	; $FFFFF7C8 ; seems unused
WindTunnel_holding_flag:	ds.b	1
Sliding_flag:			ds.b	1	; merged into the character's status flag in the final, likely to fix bugs with Tails
				ds.b	1	; $FFFFF7CB ; seems unused
Control_Locked:			ds.b	1
Enter_SpecialStage_flag:	ds.b	1
				ds.b	2	; $FFFFF7CE-$FFFFF7CF ; seems unused
Chain_Bonus_counter:		ds.w	1	; counts up when you destroy things that give points, resets when you touch the ground
Bonus_Countdown_1:		ds.w	1	; level results time bonus
Bonus_Countdown_2:		ds.w	1	; level results ring bonus
Update_Bonus_score:		ds.b	1
				ds.b	3	; $FFFFF7D7-$FFFFF7D9 ; seems unused

Camera_X_pos_coarse:		ds.w	1	; (Camera_X_pos - 128) / 256
Camera_X_pos_coarse_End:

Camera_X_pos_coarse_P2:		ds.w	1
Camera_X_pos_coarse_P2_End:

Tails_LastLoadedDPLC:		ds.b	1	; mapping frame number when Tails last had his tiles requested to be transferred from ROM to VRAM. can be set to a dummy value like -1 to force a refresh DMA.
TailsTails_LastLoadedDPLC:	ds.b	1	; mapping frame number when Tails' tails last had their tiles requested to be transferred from ROM to VRAM. can be set to a dummy value like -1 to force a refresh DMA.
ButtonVine_Trigger:		ds.b	$10	; 16 bytes flag array, #subtype byte set when button/vine of respective subtype activated
Anim_Counters:			ds.b	$10	; $FFFFF7F0-$FFFFF7FF
Misc_Variables_End:

Sprite_Table:			ds.b	$200	; Sprite attribute table buffer
Sprite_Table_End:
				; no buffer in this version, in fact there's actually not ENOUGH RAM
				; allocated to the sprite table, although in this case the "Ashura"
				; glitch only occurs with the underwater palette

; $FFFFFA00
Underwater_target_palette:		ds.b palette_line_size	; This is used by the screen-fading subroutines.
Underwater_target_palette_line2:	ds.b palette_line_size	; While Underwater_palette contains the blacked-out palette caused by the fading,
Underwater_target_palette_line3:	ds.b palette_line_size	; Underwater_target_palette will contain the palette the screen will ultimately fade in to.
Underwater_target_palette_line4:	ds.b palette_line_size

Underwater_palette:		ds.b palette_line_size	; main palette for underwater parts of the screen
Underwater_palette_line2:	ds.b palette_line_size
Underwater_palette_line3:	ds.b palette_line_size
Underwater_palette_line4:	ds.b palette_line_size

Normal_palette:			ds.b	palette_line_size	; main palette for non-underwater parts of the screen
Normal_palette_line2:		ds.b	palette_line_size
Normal_palette_line3:		ds.b	palette_line_size
Normal_palette_line4:		ds.b	palette_line_size
Normal_palette_End:

Target_palette:			ds.b	palette_line_size	; This is used by the screen-fading subroutines.
Target_palette_line2:		ds.b	palette_line_size	; While Normal_palette contains the blacked-out palette caused by the fading,
Target_palette_line3:		ds.b	palette_line_size	; Target_palette will contain the palette the screen will ultimately fade in to.
Target_palette_line4:		ds.b	palette_line_size
Target_palette_End:

Object_Respawn_Table:		; $FFFFFC00
Obj_respawn_index:		ds.b	2	; respawn table indices of the next objects when moving left or right for the first player
Obj_respawn_index_End:
Obj_respawn_data:		ds.b	$BE	; for stock S2, $80 is enough
Obj_respawn_data_End:
				ds.b	$140	; stack; the first $BE bytes are cleared by ObjectsManager_Init, with possibly disastrous consequences. At least $A0 bytes are needed.
System_Stack:

CrossResetRAM:	; RAM in this region will not be cleared after a soft reset.

				ds.b	2	; $FFFFFE00-$FFFFFE01 ; seems unused
Level_Inactive_flag:		ds.w	1	; (2 bytes)
Timer_frames:			ds.w	1	; (2 bytes)
Debug_object:			ds.b	1
				ds.b	1	; $FFFFFE07 ; seems unused
Debug_placement_mode:		ds.b	1
				ds.b	1	; the whole word is tested, but the debug mode code uses only the low byte
Debug_Accel_Timer:		ds.b	1
Debug_Speed:			ds.b	1
Vint_runcount:			ds.l	1

Current_ZoneAndAct:				; 2 bytes
Current_Zone:			ds.b	1	; 1 byte
Current_Act:			ds.b	1	; 1 byte
Life_count:			ds.b	1
				ds.b	1	; $FFFFFE13 ; seems unused
Current_Air:			ds.b	1
				ds.b	1	; $FFFFFE15 ; seems unused

Current_Special_Stage:		ds.b	1
				ds.b	1	; $FFFFFE17 ; seems unused
Continue_count:			ds.b	1	; only cleared, never used
				ds.b	1	; $FFFFFE19 ; seems unused
Time_Over_flag:			ds.b	1
Extra_life_flags:		ds.b	1

; If set, the respective HUD element will be updated.
Update_HUD_lives:		ds.b	1
Update_HUD_rings:		ds.b	1
Update_HUD_timer:		ds.b	1
Update_HUD_score:		ds.b	1

Ring_count:			ds.w	1	; 2 bytes
Timer:						; 4 bytes
				ds.b	1	; filler
Timer_minute:			ds.b	1	; 1 byte
Timer_second:			ds.b	1	; 1 byte
Timer_frame:			ds.b	1	; 1 byte

Score:				ds.l	1	; 4 bytes
				ds.b	2	; $FFFFFE2A-$FFFFFE2B ; seems unused
Shield_flag:			ds.b	1
Invincibility_flag:		ds.b	1
Speed_shoes:			ds.b	1
unk_FE2F:			ds.b	1	; cleared once, never used

Last_star_pole_hit:		ds.b	1	; 1 byte -- max activated starpole ID in this act
Saved_Last_star_pole_hit:	ds.b	1
Saved_x_pos:			ds.w	1
Saved_y_pos:			ds.w	1
Saved_Ring_count:		ds.w	1
Saved_Timer:			ds.l	1
Saved_Dynamic_Resize_Routine:	ds.b	1
				ds.b	1	; $FFFFFE3D ; seems unused
Saved_Camera_Max_Y_pos:		ds.w	1
Saved_Camera_X_pos:		ds.w	1
Saved_Camera_Y_pos:		ds.w	1
Saved_Camera_BG_X_pos:		ds.w	1
Saved_Camera_BG_Y_pos:		ds.w	1
Saved_Camera_BG2_X_pos:		ds.w	1
Saved_Camera_BG2_Y_pos:		ds.w	1
Saved_Camera_BG3_X_pos:		ds.w	1
Saved_Camera_BG3_Y_pos:		ds.w	1
Saved_Water_Level:		ds.w	1
Saved_Water_routine:		ds.b	1
Saved_Water_move:		ds.b	1
Saved_Extra_life_flags:		ds.b	1
				ds.b	2	; $FFFFFE55-$FFFFFE56 ; seems unused
Emerald_count:			ds.b	1
Got_Emeralds_array:		ds.b	6	; 8 bytes are cleared

Oscillating_Numbers:
Oscillation_Control:		ds.w	1
Oscillating_variables:
Oscillating_Data:		ds.w	$20

				; Fun Fact: when documenting the last of the ROM, I forgot to add this,
				; causing the rest to be incorrect
				ds.b	$20	; $FFFFFEA0-$FFFFFEBF ; seems unused

SpecialStage_anim_counter:
Logspike_anim_counter:		ds.b	1
SpecialStage_anim_frame:
Logspike_anim_frame:		ds.b	1
SpecialStage2_anim_counter:
Rings_anim_counter:		ds.b	1
SpecialStage2_anim_frame:
Rings_anim_frame:		ds.b	1
SpecialStage3_anim_counter:
Unknown_anim_counter:		ds.b	1
SpecialStage3_anim_frame:
Unknown_anim_frame:		ds.b	1
SpecialStage4_anim_counter:
Ring_spill_anim_counter:	ds.b	1	; scattered rings
SpecialStage4_anim_frame:
Ring_spill_anim_frame:		ds.b	1
Ring_spill_anim_accum:		ds.w	1
				ds.b	6	; $FFFFFEC9-$FFFFFECF ; seems unused
				ds.b	$20	; $FFFFFED0-$FFFFFEEF ; seems unused

Camera_Min_Y_pos_Debug_Copy:	ds.w	1
Camera_Max_Y_pos_Debug_Copy:	ds.w	1

				ds.b	$C	; unused
Oscillating_Numbers_End
				ds.b	$40	; $FFFFFEF4-$FFFFFF3F ; seems unused

Perfect_rings_left:		ds.w	1
				ds.b	$3E	; $FFFFFF42-$FFFFFF7F ; seems unused
Oscillating_variables_End

LevSel_HoldTimer:		ds.w	1
Level_select_zone:		ds.w	1
Sound_test_sound:		ds.w	1
				ds.b	$3A	; $FFFFFF86-$FFFFFFBF ; seems unused
Next_Extra_life_score:		ds.l	1
				ds.b	$C	; $FFFFFFC4-$FFFFFFCF ; seems unused

Level_select_flag:		ds.b	1
Slow_motion_flag:		ds.b	1
Debug_options_flag:		ds.b	1	; if set, allows you to enable debug mode and "night mode"
Hidden_credits_flag:		ds.b	1	; leftover from Sonic 1
Correct_cheat_entries:		ds.w	1
Correct_cheat_entries_2:	ds.w	1

Two_player_mode:		ds.w	1	; flag (0 for main game)
unk_FFDA:			ds.w	1	; cleared once at title screen, never read from
				ds.b	4	; $FFFFFFDC-$FFFFFFDF ; seems to be unused

; Values in these variables are passed to the sound driver during V-INT.
; They use a playlist index, not a sound test index.
SoundQueue STRUCT DOTS
	Music0:	ds.b	1
	SFX0:	ds.b	1
	SFX1:	ds.b	1
	SFX2:	ds.b	1 ; This one is never used, since nothing ever gets written to it.
SoundQueue ENDSTRUCT

Sound_Queue:			SoundQueue

				ds.b	$C	; $FFFFFFE4-$FFFFFFEF ; seems unused

Demo_mode_flag:			ds.w	1 ; 1 if a demo is playing (2 bytes)
Demo_number:			ds.w	1 ; which demo will play next (2 bytes)
Ending_demo_number:		ds.w	1 ; zone for the ending demos (2 bytes, unused)
				ds.w	1
Graphics_Flags:			ds.w	1 ; misc. bitfield
Debug_mode_flag:		ds.w	1 ; (2 bytes)
Checksum_fourcc:		ds.l	1 ; (4 bytes)

CrossResetRAM_End:

RAM_End

    if * > 0	; Don't declare more space than the RAM can contain!
	fatal "The RAM variable declarations are too large by $\{*} bytes."
    endif

	dephase
	!org 0

; ---------------------------------------------------------------------------
; Clocks
Master_Clock    = 53693175
M68000_Clock    = Master_Clock/7
Z80_Clock       = Master_Clock/15
FM_Sample_Rate  = M68000_Clock/(6*6*4)
PSG_Sample_Rate = Z80_Clock/16

; ---------------------------------------------------------------------------
; VDP addressses
VDP_data_port =			$C00000 ; (8=r/w, 16=r/w)
VDP_control_port =		$C00004 ; (8=r/w, 16=r/w)
PSG_input =			$C00011

; ---------------------------------------------------------------------------
; Z80 addresses
Z80_RAM =			$A00000 ; start of Z80 RAM
Z80_RAM_End =			$A02000 ; end of non-reserved Z80 RAM
Z80_Bus_Request =		$A11100
Z80_Reset =			$A11200

Security_Addr =			$A14000

; ---------------------------------------------------------------------------
; I/O Area 
HW_Version =			$A10001
HW_Port_1_Data =		$A10003
HW_Port_2_Data =		$A10005
HW_Expansion_Data =		$A10007
HW_Port_1_Control =		$A10009
HW_Port_2_Control =		$A1000B
HW_Expansion_Control =		$A1000D
HW_Port_1_TxData =		$A1000F
HW_Port_1_RxData =		$A10011
HW_Port_1_SCtrl =		$A10013
HW_Port_2_TxData =		$A10015
HW_Port_2_RxData =		$A10017
HW_Port_2_SCtrl =		$A10019
HW_Expansion_TxData =		$A1001B
HW_Expansion_RxData =		$A1001D
HW_Expansion_SCtrl =		$A1001F

; ---------------------------------------------------------------------------
; VRAM and tile art base addresses.
; VRAM Reserved regions.
VRAM_Plane_A_Name_Table				= $C000	; Extends until $CFFF
VRAM_Plane_B_Name_Table				= $E000	; Extends until $EFFF
VRAM_Plane_A_Name_Table_2P			= $A000	; Extends until $AFFF
VRAM_Plane_B_Name_Table_2P			= $8000	; Extends until $8FFF
VRAM_Plane_Window_Name_Table		= $A000 ; Extends until $FFFF
VRAM_Plane_Table_Size				= $1000	; 64 cells x 32 cells x 2 bytes per cell
VRAM_Sprite_Attribute_Table			= $F800	; Extends until $FA7F
VRAM_Sprite_Attribute_Table_Size	= $280	; 640 bytes
VRAM_Horiz_Scroll_Table				= $FC00	; Extends until $FF7F
VRAM_Horiz_Scroll_Table_Size		= $380	; 224 lines * 2 bytes per entry * 2 PNTs