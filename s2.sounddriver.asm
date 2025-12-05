; Sonic the Hedgehog 2 Simon Wai disassembled Z80 sound driver

; Disassembled by ValleyBell
; Rewritten by Filter for AS

; Technically speaking, this sound driver is basically a direct port of Sonic 1's sound driver
; from 68K to Z80. Even the push block is still fully intact here.
; ---------------------------------------------------------------------------

FixDriverBugs = FixBugs
OptimiseDriver = 0

; ---------------------------------------------------------------------------
; NOTES:
;
; This code is compressed in the ROM, but you can edit it here as uncompressed
; and it will automatically be assembled and compressed into the correct place
; during the build process.
;
; This Z80 code can use labels and equates defined in the 68k code,
; and the 68k code can use the labels and equates defined in here.
; This is fortunate, as they contain references to each other's addresses.
;
; If you want to add significant amounts of extra code to this driver,
; try putting your code as far down as possible.
; That will make you less likely to run into space shortages from dislocated data alignment.
;
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Setup defines and macros

	; zComRange:	@ 1B80h
	; 	+00h	-- Priority of current SFX (cleared when 1-up song is playing)
	; 	+01h	-- tempo clock
	; 	+02h	-- current tempo
	; 	+03h	-- Pause/unpause flag: 7Fh for pause; 80h for unpause (set from 68K)
	; 	+04h	-- total volume levels to continue decreasing volume before fade out considered complete (starts at 28h, works downward)
	; 	+05h	-- delay ticker before next volume decrease
	; 	+06h	-- communication value
	; 	+07h	-- "DAC is updating" flag (set to FFh until completion of DAC track change)
	; 	+08h	-- When NOT set to 80h, 68K request new sound index to play
	; 	+09h	-- SFX to Play queue slot
	; 	+0Ah	-- Play stereo sound queue slot
	; 	+0Bh	-- Unknown SFX Queue slot
	; 	+0Ch	-- Address to table of voices
	;
	; 	+0Eh	-- Set to 80h while fading in (disabling SFX) then 00h
	; 	+0Fh	-- Same idea as +05h, except for fade IN
	; 	+10h	-- Same idea as +04h, except for fade IN
	; 	+11h	-- 80h set indicating 1-up song is playing (stops other sounds)
	; 	+12h	-- main tempo value
	; 	+13h	-- original tempo for speed shoe restore
	; 	+14h	-- Speed shoes flag
	; 	+15h	-- If 80h, FM Channel 6 is NOT in use (DAC enabled)
	; 	+16h	-- value of which music bank to use (0 for MusicPoint1, $80 for MusicPoint2)
	; 	+17h	-- Pal mode flag
	;
	; ** zTracksSongStart starts @ +18h
	;
	; 	1B98 base
	; 	Track 1 = DAC
	; 	Then 6 FM
	; 	Then 3 PSG
	;
	;
	; 	1B98 = DAC
	; 	1BC2 = FM 1
	; 	1BEC = FM 2
	; 	1C16 = FM 3
	; 	1C40 = FM 4
	; 	1C6A = FM 5
	; 	1C94 = FM 6
	; 	1CBE = PSG 1
	; 	1CE8 = PSG 2
	; 	1D12 = PSG 3 (tone or noise)
	;
	; 	1D3C = SFX FM 3
	; 	1D66 = SFX FM 4
	; 	1D90 = SFX FM 5
	; 	1DBA = SFX PSG 1
	; 	1DE4 = SFX PSG 2
	; 	1E0E = SFX PSG 3 (tone or noise)
	;
	;
zTrack STRUCT DOTS
	; 	"playback control"; bits:
	; 	1 (02h): track is at rest
	; 	2 (04h): SFX is overriding this track
	; 	3 (08h): modulation on
	; 	4 (10h): do not attack next note
	; 	7 (80h): track is playing
	PlaybackControl:	ds.b 1
	; 	"voice control"; bits:
	; 	2 (04h): If set, bound for part II, otherwise 0 (see zWriteFMIorII)
	; 		-- bit 2 has to do with sending key on/off, which uses this differentiation bit directly
	; 	7 (80h): PSG track
	VoiceControl:		ds.b 1
	TempoDivider:		ds.b 1	; Timing divisor; 1 = Normal, 2 = Half, 3 = Third...
	DataPointerLow:		ds.b 1	; Track's position low byte
	DataPointerHigh:	ds.b 1	; Track's position high byte
	Transpose:		ds.b 1	; Transpose (from coord flag E9)
	Volume:			ds.b 1	; Channel volume (only applied at voice changes)
	AMSFMSPan:		ds.b 1	; Panning / AMS / FMS settings
	VoiceIndex:		ds.b 1	; Current voice in use OR current PSG tone
	VolFlutter:		ds.b 1	; PSG flutter (dynamically affects PSG volume for decay effects)
	StackPointer:		ds.b 1	; "Gosub" stack position offset (starts at 2Ah, i.e. end of track, and each jump decrements by 2)
	DurationTimeout:	ds.b 1	; Current duration timeout; counting down to zero
	SavedDuration:		ds.b 1	; Last set duration (if a note follows a note, this is reapplied to 0Bh)
	;
	; 	; 0Dh / 0Eh change a little depending on track -- essentially they hold data relevant to the next note to play
	SavedDAC:			; DAC: Next drum to play
	FreqLow:		ds.b 1	; FM/PSG: frequency low byte
	FreqHigh:		ds.b 1	; FM/PSG: frequency high byte
	NoteFillTimeout:	ds.b 1	; Currently set note fill; counts down to zero and then cuts off note
	NoteFillMaster:		ds.b 1	; Reset value for current note fill
	ModulationPtrLow:	ds.b 1	; Low byte of address of current modulation setting
	ModulationPtrHigh:	ds.b 1	; High byte of address of current modulation setting
	ModulationWait:		ds.b 1	; Wait for ww period of time before modulation starts
	ModulationSpeed:	ds.b 1	; Modulation speed
	ModulationDelta:	ds.b 1	; Modulation change per mod. Step
	ModulationSteps:	ds.b 1	; Number of steps in modulation (divided by 2)
	ModulationValLow:	ds.b 1	; Current modulation value low byte
	ModulationValHigh:	ds.b 1	; Current modulation value high byte
	Detune:			ds.b 1	; Set by detune coord flag E1; used to add directly to FM/PSG frequency
	VolTLMask:		ds.b 1	; zVolTLMaskTbl value set during voice setting (value based on algorithm indexing zGain table)
	PSGNoise:		ds.b 1	; PSG noise setting
	VoicePtrLow:		ds.b 1	; Low byte of custom voice table (for SFX)
	VoicePtrHigh:		ds.b 1	; High byte of custom voice table (for SFX)
	TLPtrLow:		ds.b 1	; Low byte of where TL bytes of current voice begin (set during voice setting)
	TLPtrHigh:		ds.b 1	; High byte of where TL bytes of current voice begin (set during voice setting)
	LoopCounters:		ds.b $A	; Loop counter index 0
	;   ... open ...
	GoSubStack:			; start of next track, every two bytes below this is a coord flag "gosub" (F8h) return stack
	;
	;	The bytes between +20h and +29h are "open"; starting at +20h and going up are possible loop counters
	;	(for coord flag F7) while +2Ah going down (never AT 2Ah though) are stacked return addresses going
	;	down after calling coord flag F8h.  Of course, this does mean collisions are possible with either
	;	or other track memory if you're not careful with these!  No range checking is performed!
	;
	; 	All tracks are 2Ah bytes long
zTrack ENDSTRUCT

zVar STRUCT DOTS
	SFXPriorityVal:		ds.b 1
	TempoTimeout:		ds.b 1
	CurrentTempo:		ds.b 1	; Stores current tempo value here
	StopMusic:		ds.b 1	; Set to 7Fh to pause music, set to 80h to unpause. Otherwise 00h
	FadeOutCounter:		ds.b 1
	FadeOutDelay:		ds.b 1
	Communication:		ds.b 1	; Unused byte used to synchronise gameplay events with music
	DACUpdating:		ds.b 1	; Set to FFh while DAC is updating, then back to 00h
	QueueToPlay:		ds.b 1	; The head of the queue
	Queue0:			ds.b 1
	Queue1:			ds.b 1
	Queue2:			ds.b 1	; This slot was totally broken in Sonic 1's driver. It's mostly fixed here, but it's still a little broken (see 'zInitMusicPlayback').
	VoiceTblPtr:		ds.b 2	; Address of the voices
	FadeInFlag:		ds.b 1
	FadeInDelay:		ds.b 1
	FadeInCounter:		ds.b 1
	1upPlaying:		ds.b 1
	TempoMod:		ds.b 1
	TempoTurbo:		ds.b 1	; Stores the tempo if speed shoes are acquired (or 7Bh is played otherwise)
	SpeedUpFlag:		ds.b 1
	DACEnabled:		ds.b 1
	MusicBankNumber:	ds.b 1
zVar ENDSTRUCT

; equates: standard (for Genesis games) addresses in the memory map
zYM2612_A0 =	$4000
zYM2612_D0 =	$4001
zYM2612_A1 =	$4002
zYM2612_D1 =	$4003
zBankRegister =	$6000
zPSG =		$7F11
zROMWindow =	$8000

	phase $1B80
zStack:
zAbsVar:	zVar

zTracksSongStart:	; This is the beginning of all BGM track memory
zSongDACFMStart:
zSongDAC:	zTrack
zSongFMStart:
zSongFM1:	zTrack
zSongFM2:	zTrack
zSongFM3:	zTrack
zSongFM4:	zTrack
zSongFM5:	zTrack
zSongFM6:	zTrack
zSongFMEnd:
zSongDACFMEnd:
zSongPSGStart:
zSongPSG1:	zTrack
zSongPSG2:	zTrack
zSongPSG3:	zTrack
zSongPSGEnd:
zTracksSongEnd:

zTracksSFXStart:
zSFX_FMStart:
zSFX_FM3:	zTrack
zSFX_FM4:	zTrack
zSFX_FM5:	zTrack
zSFX_FMEnd:
zSFX_PSGStart:
zSFX_PSG1:	zTrack
zSFX_PSG2:	zTrack
zSFX_PSG3:	zTrack
zSFX_PSGEnd:
zTracksSFXEnd:

zTracksSaveStart:	; When extra life plays, it backs up a large amount of memory (all track data plus 36 bytes)
zSaveVar:	zVar
zSaveSongDAC:	zTrack
zSaveSongFM1:	zTrack
zSaveSongFM2:	zTrack
zSaveSongFM3:	zTrack
zSaveSongFM4:	zTrack
zSaveSongFM5:	zTrack
zSaveSongFM6:	zTrack
zSaveSongPSG1:	zTrack
zSaveSongPSG2:	zTrack
zSaveSongPSG3:	zTrack
zTracksSaveEnd:
; See the very end for another set of variables

	if *>$2000
		fatal "Z80 variables are \{*-$2000}h bytes past the end of Z80 RAM!"
	endif
	dephase

MUSIC_TRACK_COUNT = (zTracksSongEnd-zTracksSongStart)/zTrack.len
MUSIC_DAC_FM_TRACK_COUNT = (zSongDACFMEnd-zSongDACFMStart)/zTrack.len
MUSIC_FM_TRACK_COUNT = (zSongFMEnd-zSongFMStart)/zTrack.len
MUSIC_PSG_TRACK_COUNT = (zSongPSGEnd-zSongPSGStart)/zTrack.len

SFX_TRACK_COUNT = (zTracksSFXEnd-zTracksSFXStart)/zTrack.len
SFX_FM_TRACK_COUNT = (zSFX_FMEnd-zSFX_FMStart)/zTrack.len
SFX_PSG_TRACK_COUNT = (zSFX_PSGEnd-zSFX_PSGStart)/zTrack.len

    ; In what I believe is an unfortunate design choice in AS,
    ; both the phased and unphased PCs must be within the target processor's range,
    ; which means phase is useless here despite being designed to fix this problem...
    ; oh well, I set it up to fix this later when processing the .p file
    !org 0 ; Z80 code starting at address 0 has special meaning to s2p2bin.exe

    CPU Z80UNDOC
    listing purecode

; Macro to perform a bank switch... after using this,
; the start of zROMWindow points to the start of the given 68k address,
; rounded down to the nearest $8000 byte boundary
bankswitch macro addr68k
	if OptimiseDriver
	; Because why use a and e when you can use h and l?
		ld	hl,zBankRegister+1	; +1 so that 6000h becomes 6001h, which is still a valid bankswitch port
.cnt		:= 0
		rept 9
			; this is either ld (hl),h or ld (hl),l
			db 74h|(((addr68k)&(1<<(15+.cnt)))<>0)
.cnt			:= .cnt+1
		endm
	else
		xor	a	; a = 0
		ld	e,1	; e = 1
		ld	hl,zBankRegister
.cnt		:= 0
		rept 9
			; this is either ld (hl),a or ld (hl),e
			db 73h|((((addr68k)&(1<<(15+.cnt)))=0)<<2)
.cnt			:= .cnt+1
		endm
	endif
	endm

; macro to make a certain error message clearer should you happen to get it...
rsttarget macro {INTLABEL}
	if ($&7)||($>38h)
		fatal "Function __LABEL__ is at 0\{$}h, but must be at a multiple of 8 bytes <= 38h to be used with the rst instruction."
	endif
	if "__LABEL__"<>""
__LABEL__ label $
	endif
    endm

; Function to turn a 68k address into a word the Z80 can use to access it,
; assuming the correct bank has been switched to first
zmake68kPtr function addr,zROMWindow+(addr&7FFFh)

; Function to turn a sample rate into a djnz loop counter
pcmLoopCounterBase function sampleRate,baseCycles, 1+(3579545/(sampleRate)-(baseCycles)+(13/2))/13
pcmLoopCounter function sampleRate, pcmLoopCounterBase(sampleRate,138/2) ; 138 is the number of cycles zPlaySegaSound takes to deliver two samples.
dpcmLoopCounter function sampleRate, pcmLoopCounterBase(sampleRate,297/2) ; 297 is the number of cycles zWriteToDAC takes to deliver two samples.

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Z80 'ROM' start:
; zEntryPoint:
		di	; disable interrupts
		ld	sp,zStack
		jp	zStartDAC
; ---------------------------------------------------------------------------

	if ~~OptimiseDriver
; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
		align 8
; zsub_8:
zWaitForYM:	rsttarget
		; Performs the annoying task of waiting for the FM to not be busy
		ld	a,(zYM2612_A0)
		add	a,a
		jr	c,zWaitForYM
		ret
; End of function WaitForYM
	endif

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
		align 8
; zsub_10:
zWriteFMIorII:	rsttarget
		bit	2,(ix+zTrack.VoiceControl)
		jr	z,zWriteFMI
		jr	zWriteFMII
; End of function zWriteFMIorII

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
		align 8
; zsub_18
zWriteFMI:	rsttarget
		; Write reg/data pair to part I; 'a' is register, 'c' is data
	if ~~OptimiseDriver
		push	af
		rst	zWaitForYM
		pop	af
	endif
		ld	(zYM2612_A0),a
		push	af
	if ~~OptimiseDriver
		rst	zWaitForYM
	endif
		ld	a,c
		ld	(zYM2612_D0),a
		pop	af
		ret
; End of function zWriteFMI

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
		align 8
; zsub_28:
zWriteFMII:	rsttarget
		; Write reg/data pair to part II; 'a' is register, 'c' is data
	if ~~OptimiseDriver
		push	af
		rst	zWaitForYM
		pop	af
	endif
		ld	(zYM2612_A1),a
		push	af
	if ~~OptimiseDriver
		rst	zWaitForYM
	endif
		ld	a,c
		ld	(zYM2612_D1),a
		pop	af
		ret
; End of function zWriteFMII

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
		org	38h
VInt:	rsttarget
		push	af
		exx
		call	zBankSwitchToMusic
		xor	a
		ld	(zDoSFXFlag),a	; 00 - Music Mode
		ld	ix,zAbsVar	; 1B80 - Sound RAM
		ld	a,(zAbsVar.StopMusic)	; 1B83 = Pause Mode
		or	a
		jr	z,zUpdateEverything	; 00 = not paused
		call	DoPause
		jp	RestoreDACBank
; ---------------------------------------------------------------------------

; loc_51
zUpdateEverything:
		dec	(ix+zVar.TempoTimeout)		; decrement Tempo Timeout (1B81)
		call	z,DoTempoDelay	; reached 00 - delay all tracks

		ld	a,(zAbsVar.FadeOutCounter)	; 1B84 - remaining Fade	Out Steps
		or	a
		call	nz,DoFadeOut

		ld	a,(zAbsVar.FadeInFlag)	; 1B8E - Fade In Enable
		or	a
		call	nz,DoFadeIn

		ld	a,(zAbsVar.Queue0)	; check	Sound Queue
		or	(ix+zVar.Queue1)
		or	(ix+zVar.Queue2)
		call	nz,DoSoundQueue ; at least one	of the 3 slots was filled

		ld	a,(zAbsVar.QueueToPlay)
		cp	80h
		call	nz,PlaySoundID

		ld	a,0FFh
		ld	(zAbsVar.DACUpdating),a	; 1B87 - processing DAC	channel	(FF = yes)
		ld	ix,zSongDAC	; 1B97 - Music Tracks
		bit	7,(ix+zTrack.PlaybackControl)
		call	nz,DrumUpdateTrack

		xor	a
		ld	(zAbsVar.DACUpdating),a	; 1B87 = 00 - not processing the DAC channel anymore
		ld	b,MUSIC_FM_TRACK_COUNT

loc_8F:
		push	bc
		ld	de,zTrack.len
		add	ix,de
		bit	7,(ix+zTrack.PlaybackControl)
		call	nz,UpdateFMTrack
		pop	bc
		djnz	loc_8F
		ld	b,MUSIC_PSG_TRACK_COUNT

loc_A1:
		push	bc
		ld	de,zTrack.len
		add	ix,de
		bit	7,(ix+zTrack.PlaybackControl)
		call	nz,UpdatePSGTrack
		pop	bc
		djnz	loc_A1

		bankswitch SoundIndex
		ld	a,80h
		ld	(zDoSFXFlag),a	; 00 - SFX Mode

		ld	b,SFX_FM_TRACK_COUNT

loc_C7:
		push	bc
		ld	de,zTrack.len
		add	ix,de
		bit	7,(ix+zTrack.PlaybackControl)
		call	nz,UpdateFMTrack
		pop	bc
		djnz	loc_C7

		ld	b,SFX_PSG_TRACK_COUNT

loc_D9:
		push	bc
		ld	de,zTrack.len
		add	ix,de
		bit	7,(ix+zTrack.PlaybackControl)
		call	nz,UpdatePSGTrack
		pop	bc
		djnz	loc_D9

RestoreDACBank:
		bankswitch DACSamples_Start
		ld	a,(zCurDAC)	; check, if a new DAC sound was	queued
		or	a
		jp	m,loc_105	; yes -	jump
		exx
		ld	b,1
		pop	af
		ei
		ret
; ---------------------------------------------------------------------------

loc_105:
		ld	a,80h
		ex	af,af'
		ld	a,(zCurDAC)
		sub	81h
		ld	(zCurDAC),a
		add	a,a
		add	a,a
		add	a,zDACPtrTbl&0FFh	; add lower byte from 0F75
		ld	(loc_121+1),a
		add	a,2
		ld	(loc_124+2),a
		pop	af
		ld	hl,zWriteToDAC
		ex	(sp),hl

loc_121:
		ld	hl,(zDACPtrTbl)

loc_124:
		ld	de,(zDACLenTbl)

loc_128:
		ld	bc,100h
		ei
		ret
; ---------------------------------------------------------------------------
; InitDriver:
zStartDAC:
		call	StopAllSound
		ei
		ld	iy,DPCMData
		ld	de,0
; loc_138:
zWaitLoop:
		ld	a,d		; 4
		or	e		; 4
		jr	z,zWaitLoop	; 7	; As long as 'de' (length of sample) = 0, wait...

		; 'hl' is the pointer to the sample, 'de' is the length of the sample,
		; and 'iy' points to the translation table; let's go...

		; The "djnz $" loops control the playback rate of the DAC
		; (the higher the 'b' value, the slower it will play)


		; As for the actual encoding of the data, it is described by jman2050:

		; "As for how the data is compressed, lemme explain that real quick:
		; First, it is a lossy compression. So if you recompress a PCM sample this way,
		; you will lose precision in data. Anyway, what happens is that each compressed data
		; is separated into nybbles (1 4-bit section of a byte). This first nybble of data is
		; read, and used as an index to a table containing the following data:
		; 0,1,2,4,8,$10,$20,$40,$80,$FF,$FE,$FC,$F8,$F0,$E0,$C0."   [zDACDecodeTbl / zbyte_1B3]
		; "So if the nybble were equal to F, it'd extract $C0 from the table. If it were 8,
		; it would extract $80 from the table. ... Anyway, there is also another byte of data
		; that we'll call 'd'. At the start of decompression, d is $80. What happens is that d
		; is then added to the data extracted from the table using the nybble. So if the nybble
		; were 4, the 8 would be extracted from the table, then added to d, which is $80,
		; resulting in $88. This result is then put back into d, then fed into the YM2612 for
		; processing. Then the next nybble is read, the data is extracted from the table, then
		; is added to d (remember, d is now changed because of the previous operation), then is
		; put back into d, then is fed into the YM2612. This process is repeated until the number
		; of bytes as defined in the table above are read and decompressed."

		; In our case, the so-called 'd' value is shadow register 'a'

; loc_13C:
zWriteToDAC:
		djnz	$		; 8	; Busy wait for specific amount of time in 'b'

		di			; 4	; disable interrupts (while updating DAC)
		ld	a,2Ah		; 7	; DAC port
		ld	(zYM2612_A0),a	; 13	; Set DAC port register
		ld	a,(hl)		; 7	; Get next DAC byte
		rlca			; 4
		rlca			; 4
		rlca			; 4
		rlca			; 4
		and	0Fh		; 7	; UPPER 4-bit offset into zDACDecodeTbl
		ld	(.highnybble+2),a	; 13	; store into the instruction after .highnybble (self-modifying code)
		ex	af,af'		; 4	; shadow register 'a' is the 'd' value for 'jman2050' encoding

; loc_14F
.highnybble:
		add	a,(iy+0)	; 19	; Get byte from zDACDecodeTbl (self-modified to proper index)
		ld	(zYM2612_D0),a	; 13	; Write this byte to the DAC
		ex	af,af'		; 4	; back to regular registers
		ld	b,c		; 4	; reload 'b' with wait value
		ei			; 4	; enable interrupts (done updating DAC, busy waiting for next update)
		nop			; 4

		djnz	$		; 8	; Busy wait for specific amount of time in 'b'

		di			; 4	; disable interrupts (while updating DAC)
		push	af		; 11
		pop	af		; 11
		ld	a,2Ah		; 7	; DAC port
		ld	(zYM2612_A0),a	; 13	; Set DAC port register
		ld	b,c		; 4	; reload 'b' with wait value
		ld	a,(hl)		; 7	; Get next DAC byte
		inc	hl		; 6	; Next byte in DAC stream...
		dec	de		; 6	; One less byte
		and	0Fh		; 7	; LOWER 4-bit offset into zDACDecodeTbl
		ld	(.lownybble+2),a	; 13	; store into the instruction after .lownybble (self-modifying code)
		ex	af,af'		; 4	; shadow register 'a' is the 'd' value for 'jman2050' encoding

; loc_16D
.lownybble:
		add	a,(iy+0)	; 19	; Get byte from zDACDecodeTbl (self-modified to proper index)
		ld	(zYM2612_D0),a	; 13	; Write this byte to the DAC
		ex	af,af'		; 4	; back to regular registers
		ei			; 4	; enable interrupts (done updating DAC, busy waiting for next update)
		nop			; 4
		jp	zWaitLoop	; 10	; Back to the wait loop; if there's more DAC to write, we come back down again!
					; 297
		; 297 cycles for two samples. dpcmLoopCounter should use 297 divided by 2.
; ---------------------------------------------------------------------------
DPCMData:
		db 0,1,2,4,8,10h,20h,40h
		db 80h,-1,-2,-4,-8,-10h,-20h,-40h
BGMChnPtrs:
		dw zSongFM3
		dw 0
		dw zSongFM4
		dw zSongFM5
		dw zSongPSG1
		dw zSongPSG2
		dw zSongPSG3
		dw zSongPSG3
SFXChnPtrs:
		dw zSFX_FM3
		dw 0
		dw zSFX_FM4
		dw zSFX_FM5
		dw zSFX_PSG1
		dw zSFX_PSG2
		dw zSFX_PSG3
		dw zSFX_PSG3
; ---------------------------------------------------------------------------

DrumUpdateTrack:
		dec	(ix+zTrack.DurationTimeout)
		ret	nz
		ld	l,(ix+zTrack.DataPointerLow)
		ld	h,(ix+zTrack.DataPointerHigh)

loc_1B3:
		ld	a,(hl)
		inc	hl
		cp	0E0h
		jr	c,loc_1BF
		call	zCoordFlag
		jp	loc_1B3
; ---------------------------------------------------------------------------

loc_1BF:
		or	a
		jp	p,loc_1D5
		ld	(ix+zTrack.SavedDAC),a
		ld	a,(hl)
		or	a
		jp	p,loc_1D4
		ld	a,(ix+zTrack.SavedDuration)
		ld	(ix+zTrack.DurationTimeout),a
		jp	loc_1D8
; ---------------------------------------------------------------------------

loc_1D4:
		inc	hl

loc_1D5:
		call	TickMultiplier

loc_1D8:
		ld	(ix+zTrack.DataPointerLow),l
		ld	(ix+zTrack.DataPointerHigh),h
		bit	2,(ix+zTrack.PlaybackControl)
		ret	nz
		ld	a,(ix+zTrack.SavedDAC)
		cp	80h
		ret	z		; Drum 80 (null-drum) -	return
		sub	81h
		add	a,a		; else look up the DAC playlist
		add	a,zDACMasterPlaylist&0FFh
		ld	(loc_1F1+2),a

loc_1F1:
		ld	bc,(zDACMasterPlaylist)
		ld	a,c
		ld	(zCurDAC),a	; request new DAC sound	to be played
		ld	a,b
		ld	(loc_128+1),a	; set playback speed
		ret

; =============== S U B	R O U T	I N E =======================================


UpdateFMTrack:
		dec	(ix+zTrack.DurationTimeout)
		jr	nz,loc_210
		res	4,(ix+zTrack.PlaybackControl)
		call	TrkUpdate_FM
		call	SendFMFreq
		jp	DoNoteOn
; ---------------------------------------------------------------------------

loc_210:
		call	DoNoteStop
		call	DoModulation
		jp	RefreshFMFreq
; End of function UpdateFMTrack


; =============== S U B	R O U T	I N E =======================================


TrkUpdate_FM:
		ld	l,(ix+zTrack.DataPointerLow)
		ld	h,(ix+zTrack.DataPointerHigh)
		res	1,(ix+zTrack.PlaybackControl)

loc_223:
		ld	a,(hl)
		inc	hl
		cp	0E0h
		jr	c,loc_22F
		call	zCoordFlag
		jp	loc_223
; ---------------------------------------------------------------------------

loc_22F:
		push	af
		call	DoNoteOff
		pop	af
		or	a
		jp	p,loc_241
		call	GetFMFreq
		ld	a,(hl)
		or	a
		jp	m,FinishTrkUpdate
		inc	hl

loc_241:
		call	TickMultiplier
		jp	FinishTrkUpdate
; End of function TrkUpdate_FM

; ---------------------------------------------------------------------------

GetFMFreq:
		sub	80h
		jr	z,loc_25F
		add	a,(ix+zTrack.Transpose)
		add	a,a
		add	a,FMFreqs&0FFh
		ld	(loc_254+2),a

loc_254:
		ld	de,(FMFreqs)
		ld	(ix+zTrack.FreqLow),e
		ld	(ix+zTrack.FreqHigh),d
		ret
; ---------------------------------------------------------------------------

loc_25F:
		set	1,(ix+zTrack.PlaybackControl)
		xor	a
		ld	(ix+zTrack.FreqLow),a
		ld	(ix+zTrack.FreqHigh),a
		ret

; =============== S U B	R O U T	I N E =======================================


TickMultiplier:
		ld	c,a
		ld	b,(ix+zTrack.TempoDivider)

loc_26F:
		djnz	loc_278
		ld	(ix+zTrack.SavedDuration),a
		ld	(ix+zTrack.DurationTimeout),a
		ret
; ---------------------------------------------------------------------------

loc_278:
		add	a,c
		jp	loc_26F
; End of function TickMultiplier

; ---------------------------------------------------------------------------

FinishTrkUpdate:
		ld	(ix+zTrack.DataPointerLow),l
		ld	(ix+zTrack.DataPointerHigh),h
		ld	a,(ix+zTrack.SavedDuration)
		ld	(ix+zTrack.DurationTimeout),a
		bit	4,(ix+zTrack.PlaybackControl)
		ret	nz
		ld	a,(ix+zTrack.NoteFillMaster)
		ld	(ix+zTrack.NoteFillTimeout),a
		ld	(ix+zTrack.VolFlutter),0
		bit	3,(ix+zTrack.PlaybackControl)
		ret	z
		ld	l,(ix+zTrack.ModulationPtrLow)
		ld	h,(ix+zTrack.ModulationPtrHigh)
		jp	loc_E18

; =============== S U B	R O U T	I N E =======================================


DoNoteStop:
		ld	a,(ix+zTrack.NoteFillTimeout)
		or	a
		ret	z
		dec	(ix+zTrack.NoteFillTimeout)
		ret	nz
		set	1,(ix+zTrack.PlaybackControl)
		pop	de
		bit	7,(ix+zTrack.VoiceControl)
		jp	nz,PSGNoteOff
		jp	DoNoteOff
; End of function DoNoteStop


; =============== S U B	R O U T	I N E =======================================


DoModulation:
		pop	de
		bit	1,(ix+zTrack.PlaybackControl)
		ret	nz
		bit	3,(ix+zTrack.PlaybackControl)
		ret	z
		ld	a,(ix+zTrack.ModulationWait)
		or	a
		jr	z,loc_2D2
		dec	(ix+zTrack.ModulationWait)
		ret
; ---------------------------------------------------------------------------

loc_2D2:
		dec	(ix+zTrack.ModulationSpeed)
		ret	nz
		ld	l,(ix+zTrack.ModulationPtrLow)
		ld	h,(ix+zTrack.ModulationPtrHigh)
		inc	hl
		ld	a,(hl)
		ld	(ix+zTrack.ModulationSpeed),a
		ld	a,(ix+zTrack.ModulationSteps)
		or	a
		jr	nz,loc_2F6
		inc	hl
		inc	hl
		ld	a,(hl)
		ld	(ix+zTrack.ModulationSteps),a
		ld	a,(ix+zTrack.ModulationDelta)
		neg
		ld	(ix+zTrack.ModulationDelta),a
		ret
; ---------------------------------------------------------------------------

loc_2F6:
		dec	(ix+zTrack.ModulationSteps)
		ld	l,(ix+zTrack.ModulationValLow)
		ld	h,(ix+zTrack.ModulationValHigh)
		ld	b,0
		ld	c,(ix+zTrack.ModulationDelta)
		bit	7,c
		jp	z,loc_30B
		ld	b,0FFh

loc_30B:
		add	hl,bc
		ld	(ix+zTrack.ModulationValLow),l
		ld	(ix+zTrack.ModulationValHigh),h
		ld	c,(ix+zTrack.FreqLow)
		ld	b,(ix+zTrack.FreqHigh)
		add	hl,bc
		ex	de,hl
		jp	(hl)
; End of function DoModulation

; ---------------------------------------------------------------------------
FMFreqs:
		dw 25Eh,284h,2ABh,2D3h,2FEh,32Dh,35Ch,38Fh,3C5h,3FFh,43Ch,47Ch
		dw 0A5Eh,0A84h,0AABh,0AD3h,0AFEh,0B2Dh,0B5Ch,0B8Fh,0BC5h,0BFFh,0C3Ch,0C7Ch
		dw 125Eh,1284h,12ABh,12D3h,12FEh,132Dh,135Ch,138Fh,13C5h,13FFh,143Ch,147Ch
		dw 1A5Eh,1A84h,1AABh,1AD3h,1AFEh,1B2Dh,1B5Ch,1B8Fh,1BC5h,1BFFh,1C3Ch,1C7Ch
		dw 225Eh,2284h,22ABh,22D3h,22FEh,232Dh,235Ch,238Fh,23C5h,23FFh,243Ch,247Ch
		dw 2A5Eh,2A84h,2AABh,2AD3h,2AFEh,2B2Dh,2B5Ch,2B8Fh,2BC5h,2BFFh,2C3Ch,2C7Ch
		dw 325Eh,3284h,32ABh,32D3h,32FEh,332Dh,335Ch,338Fh,33C5h,33FFh,343Ch,347Ch
		dw 3A5Eh,3A84h,3AABh,3AD3h,3AFEh,3B2Dh,3B5Ch,3B8Fh,3BC5h,3BFFh,3C3Ch,3C7Ch

; =============== S U B	R O U T	I N E =======================================


SendFMFreq:
		bit	1,(ix+zTrack.PlaybackControl)
		ret	nz
		ld	e,(ix+zTrack.FreqLow)
		ld	d,(ix+zTrack.FreqHigh)
		ld	a,d
		or	e
		jp	z,zSetRest

RefreshFMFreq:
		bit	2,(ix+zTrack.PlaybackControl)
		ret	nz
		ld	h,0
		ld	l,(ix+zTrack.Detune)
		bit	7,l
		jr	z,loc_3FB
		ld	h,0FFh

loc_3FB:
		add	hl,de
		ld	c,h
		ld	a,(ix+zTrack.VoiceControl)
		and	3
		add	a,0A4h
		rst	zWriteFMIorII
		ld	c,l
		sub	4
	if OptimiseDriver
		jp	zWriteFMIorII
	else
		rst	zWriteFMIorII
		ret
	endif
; End of function SendFMFreq

; ---------------------------------------------------------------------------
zPSGFrequencies:
		dw 356h,326h,2F9h,2CEh,2A5h,280h,25Ch,23Ah,21Ah,1FBh,1DFh,1C4h
		dw 1ABh,193h,17Dh,167h,153h,140h,12Eh,11Dh,10Dh,0FEh,0EFh,0E2h
		dw 0D6h,0C9h,0BEh,0B4h,0A9h,0A0h,97h,8Fh,87h,7Fh,78h,71h
		dw 6Bh,65h,5Fh,5Ah,55h,50h,4Bh,47h,43h,40h,3Ch,39h
		dw 36h,33h,30h,2Dh,2Bh,28h,26h,24h,22h,20h,1Fh,1Dh
		dw 1Bh,1Ah,18h,17h,16h,15h,13h,12h,11h,0

; =============== S U B	R O U T	I N E =======================================


UpdatePSGTrack:
		dec	(ix+zTrack.DurationTimeout)
		jr	nz,loc_4A8
		res	4,(ix+zTrack.PlaybackControl)
		call	TrkUpdate_PSG
		call	SendPSGFreq
		jp	zPSGDoVolFX
; ---------------------------------------------------------------------------

loc_4A8:
		call	DoNoteStop
		call	zPSGUpdateVolFX
		call	DoModulation
		jp	RefreshPSGFreq
; End of function UpdatePSGTrack


; =============== S U B	R O U T	I N E =======================================


TrkUpdate_PSG:
		ld	l,(ix+zTrack.DataPointerLow)
		ld	h,(ix+zTrack.DataPointerHigh)
		res	1,(ix+zTrack.PlaybackControl)

loc_4BE:
		ld	a,(hl)
		inc	hl
		cp	0E0h
		jr	c,loc_4CA
		call	zCoordFlag
		jp	loc_4BE
; ---------------------------------------------------------------------------

loc_4CA:
		or	a
		jp	p,loc_4D7
		call	GetPSGFreq
		ld	a,(hl)
		or	a
		jp	m,FinishTrkUpdate
		inc	hl

loc_4D7:
		call	TickMultiplier
		jp	FinishTrkUpdate
; End of function TrkUpdate_PSG

; ---------------------------------------------------------------------------

GetPSGFreq:
		sub	81h
		jr	c,loc_4F5
		add	a,(ix+zTrack.Transpose)
		add	a,a
		add	a,zPSGFrequencies&0FFh
		ld	(loc_4EA+2),a

loc_4EA:
		ld	de,(zPSGFrequencies)
		ld	(ix+zTrack.FreqLow),e
		ld	(ix+zTrack.FreqHigh),d
		ret
; ---------------------------------------------------------------------------

loc_4F5:
		set	1,(ix+zTrack.PlaybackControl)
		ld	a,0FFh
		ld	(ix+zTrack.FreqLow),a
		ld	(ix+zTrack.FreqHigh),a
		jp	PSGNoteOff

; =============== S U B	R O U T	I N E =======================================


SendPSGFreq:
		bit	7,(ix+zTrack.FreqHigh)
		jr	nz,zSetRest
		ld	e,(ix+zTrack.FreqLow)
		ld	d,(ix+zTrack.FreqHigh)

RefreshPSGFreq:
		ld	a,(ix+zTrack.PlaybackControl)
		and	6
		ret	nz
		ld	h,0
		ld	l,(ix+zTrack.Detune)
		bit	7,l
		jr	z,loc_521
		ld	h,0FFh

loc_521:
		add	hl,de
		ld	a,(ix+zTrack.VoiceControl)
		cp	0E0h
		jr	nz,loc_52B
		ld	a,0C0h

loc_52B:
		ld	b,a
		ld	a,l
		and	0Fh
		or	b
		ld	(zPSG),a
		ld	a,l
		srl	h
		rra
		srl	h
		rra
		rra
		rra
		and	3Fh
		ld	(zPSG),a
		ret
; ---------------------------------------------------------------------------

zSetRest:
		set	1,(ix+zTrack.PlaybackControl)
		ret
; End of function SendPSGFreq


; =============== S U B	R O U T	I N E =======================================


zPSGUpdateVolFX:
		ld	a,(ix+zTrack.VoiceIndex)
		or	a
		ret	z

zPSGDoVolFX:
		ld	b,(ix+zTrack.Volume)
		ld	a,(ix+zTrack.VoiceIndex)
		or	a
		jr	z,zPSGUpdateVol
		ld	hl,VolEnvPtrs
		dec	a
		add	a,a
		ld	e,a
		ld	d,0
		add	hl,de
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		add	a,(ix+zTrack.VolFlutter)
		ld	l,a
		adc	a,h
		sub	l
		ld	h,a
		ld	a,(hl)
		inc	(ix+zTrack.VolFlutter)
		or	a
		jp	p,loc_574
		cp	80h
		jr	z,zVolEnvHold

loc_574:
		add	a,b
		cp	10h
		jr	c,loc_57B
		ld	a,0Fh

loc_57B:
		ld	b,a
; End of function zPSGUpdateVolFX


; =============== S U B	R O U T	I N E =======================================


zPSGUpdateVol:
		ld	a,(ix+zTrack.PlaybackControl)
		and	6
		ret	nz
		bit	4,(ix+zTrack.PlaybackControl)
		jr	nz,zPSGCheckNoteFill

zPSGSendVol:
		ld	a,(ix+zTrack.VoiceControl)
		or	b
		add	a,10h
		ld	(zPSG),a
		ret
; ---------------------------------------------------------------------------

zPSGCheckNoteFill:
		ld	a,(ix+zTrack.NoteFillMaster)
		or	a
		jr	z,zPSGSendVol
		ld	a,(ix+zTrack.NoteFillTimeout)
		or	a
		jr	nz,zPSGSendVol
		ret
; End of function zPSGUpdateVol

; ---------------------------------------------------------------------------

zVolEnvHold:
	if FixDriverBugs
		dec	(ix+zTrack.VolFlutter)
		dec	(ix+zTrack.VolFlutter)
		jp	zPSGDoVolFX
	else
		dec	(ix+zTrack.VolFlutter)
		ret
	endif

; =============== S U B	R O U T	I N E =======================================


PSGNoteOff:
		bit	2,(ix+zTrack.PlaybackControl)
		ret	nz
		ld	a,(ix+zTrack.VoiceControl)
		or	1Fh
		ld	(zPSG),a
		ret
; End of function PSGNoteOff

; ---------------------------------------------------------------------------

SilencePSG:
		ld	hl,zPSG
		ld	(hl),9Fh
		ld	(hl),0BFh
		ld	(hl),0DFh
		ld	(hl),0FFh
		ret

; =============== S U B	R O U T	I N E =======================================


DoPause:
		jp	m,UnpauseMusic	; 80-FF	- request Unpause
		cp	2		; 02 - already paused?
		ret	z		; yes -	return
		ld	(ix+zVar.StopMusic),2	; 01 - request Pause,set to 02
		call	SilenceFM
		jp	SilencePSG
; ---------------------------------------------------------------------------

UnpauseMusic:
    if OptimiseDriver
		xor	a			; a = 0
		ld	(zAbsVar.StopMusic),a	; Clear pause/unpause flag
    else
		push	ix			; Save ix (nothing uses this, beyond this point...)
		ld	(ix+zVar.StopMusic),0	; Clear pause/unpause flag
    endif
		ld	ix,zSongDACFMStart
		ld	b,MUSIC_DAC_FM_TRACK_COUNT
		call	zResumeTrack
		bankswitch SoundIndex
		ld	ix,zSFX_FMStart
		ld	b,SFX_FM_TRACK_COUNT
    if OptimiseDriver
		; Fall-through to zResumeTrack...
    else
		call	zResumeTrack
		; None of this is necessary...
		call	zBankSwitchToMusic	; Back to music (Pointless: music isn't updated until the next frame)
		pop	ix			; Restore ix (nothing uses this, beyond this point...)
		ret
    endif
; End of function DoPause


; =============== S U B	R O U T	I N E =======================================


zResumeTrack:
		bit	7,(ix+zTrack.PlaybackControl)
		jr	z,.nexttrack
		bit	2,(ix+zTrack.PlaybackControl)
		jr	nz,.nexttrack
    if ~~OptimiseDriver
		; cfSetVoiceCont already does this
		ld	c,(ix+zTrack.AMSFMSPan)		; AMS/FMS/panning flags
		ld	a,(ix+zTrack.VoiceControl)	; Get voice control bits...
		and	3				; ... the FM portion of them
		add	a,0B4h				; Command to select AMS/FMS/panning register
		rst	zWriteFMIorII
    endif
		push	bc
		ld	a,(ix+zTrack.VoiceIndex)
		call	zSetVoiceMusic
		pop	bc

.nexttrack:
		ld	de,zTrack.len
		add	ix,de
		djnz	zResumeTrack
		ret
; End of function zResumeTrack


; =============== S U B	R O U T	I N E =======================================


DoSoundQueue:
		ld	a,(zAbsVar.QueueToPlay)
		cp	80h
		ret	nz		; Play Sound slot is full - return
		ld	hl,zAbsVar.Queue0
		ld	a,(zAbsVar.SFXPriorityVal)	; 1B80 - current SFX Priority
		ld	c,a
		ld	b,3

loc_630:
		ld	a,(hl)
		ld	e,a
		ld	(hl),0
		inc	hl
		cp	MusID__First
		jr	c,loc_65B
		sub	SndID__First
		jr	nc,loc_642
		ld	a,e
		ld	(zAbsVar.QueueToPlay),a
		ret
; ---------------------------------------------------------------------------

loc_642:
		push	hl
		add	a,SndPriorities&0FFh		; add lower byte of 0F30 (SndPriorities)
		ld	l,a
		adc	a,(SndPriorities&0FF00h)>>8	; higher byte of 0F30 (SndPriorities)
		sub	l
		ld	h,a
		ld	a,(hl)
		cp	c
		jr	c,loc_653
		ld	c,a
		ld	a,e
		ld	(zAbsVar.QueueToPlay),a

loc_653:
		pop	hl
		ld	a,c
		or	a
		ret	m
		ld	(zAbsVar.SFXPriorityVal),a
		ret
; ---------------------------------------------------------------------------

loc_65B:
		djnz	loc_630
		ret
; End of function DoSoundQueue

; ---------------------------------------------------------------------------

PlaySoundID:
		or	a
		jp	z,StopAllSound
		ret	p		; 00-7F	- Stop All
		ld	(ix+zVar.QueueToPlay),80h
		cp	MusID__End
		jp	c,zPlayMusic	; 80-9F	- Music
		cp	SndID__First
		ret	c
		cp	SndID__End
		jp	c,PlaySFX	; A0-E0	- SFX
		cp	CmdID__First
		ret	c		; E2-F8	- unused
		cp	CmdID__End
		ret	nc		; FE-FF	- reserved for pausing/unpausing music
		sub	CmdID__First	; F9-FD	- Special Commands
		add	a,a
		add	a,a
		ld	(.commandjump+1),a
; loc_681
.commandjump:
		jr	$
; ---------------------------------------------------------------------------
zCommandIndex:
CmdPtr_FadeOut:		jp	FadeOutMusic	; F9
			db	0
CmdPtr_SegaSound:	jp	PlaySegaSound	; FA
			db	0
CmdPtr_SpeedUp:		jp	SpeedUpMusic	; FB
			db	0
CmdPtr_SlowDown:	jp	SlowDownMusic	; FC
			db	0
CmdPtr_Stop:		jp	StopAllSound	; FD
			db	0
CmdPtr__End:
; ---------------------------------------------------------------------------

PlaySegaSound:
		ld	a,2Bh		; DAC enable/disable register
		ld	c,80h		; Command to enable DAC
		rst	zWriteFMI

		bankswitch Sega_Snd	; We want the Sega sound

		ld	hl,zmake68kPtr(Sega_Snd) ; was 9E8Ch
		ld	de,(Sega_Snd_End-Sega_Snd)/2	; was: 30BAh
		ld	a,2Ah			; DAC data register
		ld	(zYM2612_A0),a		; Select it
		ld	c,80h			; If QueueToPlay is not this, stops Sega PCM

loc_6B8:
		ld	a,(hl)			; 7	; Get next PCM byte
		ld	(zYM2612_D0),a		; 13	; Send to DAC
		inc	hl			; 6	; Advance pointer
		nop				; 4
		ld	b,pcmLoopCounter(16500)	; 7	; Sega PCM pitch

loc_6C0:
		djnz	$			; 8	; Delay loop
		ld	a,(zAbsVar.QueueToPlay)	; 13	; Get next item to play
		cp	c			; 4	; Is it 80h?
		jr	nz,loc_6D8		; 7	; If not, stop Sega PCM
		ld	a,(hl)			; 7	; Get next PCM byte
		ld	(zYM2612_D0),a		; 13	; Send to DAC
		inc	hl			; 6	; Advance pointer
		nop				; 4
		ld	b,pcmLoopCounter(16500)	; 7	; Sega PCM pitch

loc_6D0:
		djnz	$			; 8	; Delay loop
		dec	de			; 6	; 2 less bytes to play
		ld	a,d			; 4	; a = d
		or	e			; 4	; Is de zero?
		jp	nz,loc_6B8		; 10	; If not, loop
						; 138
		; Two samples per 138 cycles, meaning that pcmLoopCounter should used 138 divided by 2.

loc_6D8:
		call	zBankSwitchToMusic
		ld	a,(zAbsVar.DACEnabled)	; load DAC State
		ld	c,a
		ld	a,2Bh		; Reg 02B - DAC	Enable/Disable
	if OptimiseDriver
		jp	zWriteFMI
	else
		rst	zWriteFMI
		ret
	endif
; ---------------------------------------------------------------------------

zPlayMusic:
		ld	(byte_11A9),a	; make a backup	of the Music ID
		cp	MusID_ExtraLife
		jr	nz,loc_725
		ld	a,(zAbsVar.1upPlaying)
		or	a
		jr	nz,loc_72C
		ld	ix,zTracksSongStart
		ld	de,zTrack.len
		ld	b,MUSIC_TRACK_COUNT

loc_6F9:
		res	2,(ix+zTrack.PlaybackControl)
		add	ix,de
		djnz	loc_6F9
		ld	ix,zTracksSFXStart
		ld	b,SFX_TRACK_COUNT

loc_707:
		res	7,(ix+zTrack.PlaybackControl)
		add	ix,de
		djnz	loc_707
		ld	de,zTracksSaveStart
		ld	hl,zAbsVar
		ld	bc,zTracksSaveEnd-zTracksSaveStart
		ldir
		ld	a,80h
		ld	(zAbsVar.1upPlaying),a
		xor	a
		ld	(zAbsVar.SFXPriorityVal),a
		jr	loc_72C
; ---------------------------------------------------------------------------

loc_725:
		xor	a
		ld	(zAbsVar.1upPlaying),a
		ld	(zAbsVar.FadeInCounter),a

loc_72C:
		call	sub_AAE
		ld	a,(byte_11A9)	; read Music ID	back
		sub	MusID__First
		ld	e,a
		ld	d,0
		ld	hl,SpeedUpTempoLst
		add	hl,de
		ld	a,(hl)
		ld	(zAbsVar.TempoTurbo),a
		ld	hl,zMasterPlaylist
		add	hl,de
		ld	a,(hl)
		ld	b,a
		and	80h
		ld	(zAbsVar.MusicBankNumber),a	; write	Music Bank byte
		ld	a,b
		add	a,a
		ld	e,a
		ld	d,0
		ld	hl,zmake68kPtr(MusicPoint2)
		add	hl,de
		push	hl
		call	zBankSwitchToMusic
		pop	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		push	de
		pop	ix
		ld	e,(ix+zTrack.PlaybackControl)
		ld	d,(ix+zTrack.VoiceControl)
		ld	(zAbsVar.VoiceTblPtr),de
		ld	a,(ix+zTrack.Transpose)
		ld	(zAbsVar.TempoMod),a
		ld	b,a
		ld	a,(zAbsVar.SpeedUpFlag)
		or	a
		ld	a,b
		jr	z,loc_779
		ld	a,(zAbsVar.TempoTurbo)

loc_779:
		ld	(zAbsVar.CurrentTempo),a
		ld	(zAbsVar.TempoTimeout),a
		push	ix
		pop	hl
		ld	de,6
		add	hl,de
		ld	a,(ix+2)
		or	a
		jp	z,loc_7F9
		ld	b,a
		push	iy
		ld	iy,zSongDAC	; 1B97 - Music Tracks
		ld	c,(ix+zTrack.DataPointerHigh)
		ld	de,FMInitBytes

loc_79A:
		set	7,(iy+zTrack.PlaybackControl)
		ld	a,(de)
		inc	de
		ld	(iy+zTrack.VoiceControl),a
		ld	(iy+zTrack.TempoDivider),c
		ld	(iy+zTrack.StackPointer),2Ah
		ld	(iy+zTrack.AMSFMSPan),0C0h
		ld	(iy+zTrack.DurationTimeout),1
		push	de
		push	bc
		ld	a,iyl
		add	a,3
		ld	e,a
		adc	a,iyh
		sub	e
		ld	d,a
		ldi
		ldi
		ldi
		ldi
		ld	de,zTrack.len
		add	iy,de
		pop	bc
		pop	de
		djnz	loc_79A
		pop	iy
		ld	a,(ix+zTrack.TempoDivider)
		cp	7
		jr	nz,loc_7DB
		xor	a
		ld	c,a
		jr	loc_7F3
; ---------------------------------------------------------------------------

loc_7DB:
		ld	a,28h
		ld	c,6
		rst	zWriteFMI
		ld	a,42h
		ld	c,0FFh
		ld	b,4

loc_7E6:
		rst	zWriteFMII
		add	a,4
		djnz	loc_7E6
		ld	a,0B6h
		ld	c,0C0h
		rst	zWriteFMII
		ld	a,80h
		ld	c,a

loc_7F3:
		ld	(zAbsVar.DACEnabled),a
		ld	a,2Bh
		rst	zWriteFMI

loc_7F9:
		ld	a,(ix+zTrack.DataPointerLow)
		or	a
		jp	z,loc_845
		ld	b,a
		push	iy
		ld	iy,zSongPSG1
		ld	c,(ix+zTrack.DataPointerHigh)
		ld	de,PSGInitBytes

loc_80D:
		set	7,(iy+zTrack.PlaybackControl)
		ld	a,(de)
		inc	de
		ld	(iy+zTrack.VoiceControl),a
		ld	(iy+zTrack.TempoDivider),c
		ld	(iy+zTrack.StackPointer),2Ah
		ld	(iy+zTrack.DurationTimeout),1
		push	de
		push	bc
		ld	a,iyl
		add	a,3
		ld	e,a
		adc	a,iyh
		sub	e
		ld	d,a
		ldi
		ldi
		ldi
		ldi
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	(iy+zTrack.VoiceIndex),	a
		ld	de,zTrack.len
		add	iy,de
		pop	bc
		pop	de
		djnz	loc_80D
		pop	iy

loc_845:
		ld	ix,zTracksSFXStart
		ld	b,SFX_TRACK_COUNT
		ld	de,zTrack.len

loc_84E:
		bit	7,(ix+zTrack.PlaybackControl)
		jr	z,loc_870
		ld	a,(ix+zTrack.VoiceControl)
		or	a
		jp	m,loc_860
		sub	2
		add	a,a
		jr	loc_866
; ---------------------------------------------------------------------------

loc_860:
		rra
		rra
		rra
		rra
		and	0Fh

loc_866:
		add	a,BGMChnPtrs&0FFh
		ld	(loc_86B+1),a

loc_86B:
		ld	hl,(BGMChnPtrs)
		res	2,(hl)

loc_870:
		add	ix,de
		djnz	loc_84E
		ld	ix,zSongFMStart	; 1BC1 - Music Track FM	1
		ld	b,MUSIC_FM_TRACK_COUNT

loc_87A:
		call	DoNoteOff
		add	ix,de
		djnz	loc_87A
		ld	b,MUSIC_PSG_TRACK_COUNT

loc_883:
		call	PSGNoteOff
		add	ix,de
		djnz	loc_883
		ret
; ---------------------------------------------------------------------------
FMInitBytes:
		db 6,0,1,2,4,5,6
PSGInitBytes:
		db 80h,0A0h,0C0h
; ---------------------------------------------------------------------------

PlaySFX:
		ld	c,a
		ld	a,(ix+zTrack.ModulationPtrLow)
		or	(ix+zTrack.DataPointerHigh)
		or	(ix+zTrack.FreqHigh)
		jp	nz,sub_978
		ld	a,c
		cp	SndID_RingRight
		jr	nz,loc_8B6
		ld	a,(byte_11AB)	; check	Ring Speaker
		or	a
		jr	nz,loc_8AF
		ld	c,SndID_RingLeft	; change SFX ID,play on left speaker

loc_8AF:
		cpl
		ld	(byte_11AB),a	; write	inverted Ring Speaker value back
		jp	loc_8C5
; ---------------------------------------------------------------------------

loc_8B6:
		ld	a,c
		cp	SndID_PushBlock
		jr	nz,loc_8C5
		ld	a,(zPushingFlag)
		or	a
		ret	nz		; Pushing sound	not yet	finished - prevent from	playing	again
		ld	a,80h
		ld	(zPushingFlag),a	; set Pushing Flag

loc_8C5:
		bankswitch SoundIndex
		ld	hl,zmake68kPtr(SoundIndex)
		ld	a,c
		sub	SndID__First
		add	a,a
		ld	e,a
		ld	d,0
		add	hl,de
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	(loc_967+1),de
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		inc	hl

loc_8EF:
		push	bc
		xor	a
		ld	(loc_95E+1),a
		push	hl
		inc	hl
		ld	a,(hl)
		or	a
		jp	m,loc_901
		sub	2
		add	a,a
		jp	loc_91A
; ---------------------------------------------------------------------------

loc_901:
		ld	(loc_95E+1),a
		cp	0C0h
		jr	nz,loc_914
		push	af
		or	1Fh
		ld	(zPSG),a
		xor	20h
		ld	(zPSG),a
		pop	af

loc_914:
		rra
		rra
		rra
		rra
		and	0Fh

loc_91A:
		add	a,BGMChnPtrs&0FFh
		ld	(loc_91F+1),a

loc_91F:
		ld	hl,(BGMChnPtrs)
		set	2,(hl)
		add	a,SFXChnPtrs-BGMChnPtrs
		ld	(loc_929+2),a

loc_929:
		ld	ix,(SFXChnPtrs)
		ld	e,ixl
		ld	d,ixh
		push	de
		ld	l,e
		ld	h,d
		ld	(hl),0
		inc	de
		ld	bc,zTrack.len-1
		ldir
		pop	de
		pop	hl
		ldi
		ldi
		pop	bc
		push	bc
		ld	(ix+zTrack.TempoDivider),c
		ld	(ix+zTrack.DurationTimeout),1
		ld	(ix+zTrack.StackPointer),2Ah
		ld	a,e
		add	a,1
		ld	e,a
		adc	a,d
		sub	e
		ld	d,a
		ldi
		ldi
		ldi
		ldi

loc_95E:
		ld	a,0
		or	a
		jr	nz,loc_970
		ld	(ix+zTrack.AMSFMSPan),0C0h

loc_967:
		ld	de,0
		ld	(ix+zTrack.VoicePtrLow),e
		ld	(ix+zTrack.VoicePtrHigh),d

loc_970:
		pop	bc
		dec	b
		jp	nz,loc_8EF
		jp	zBankSwitchToMusic

; =============== S U B	R O U T	I N E =======================================


sub_978:
		xor	a
		ld	(zAbsVar.SFXPriorityVal),a
		ret
; End of function sub_978


; =============== S U B	R O U T	I N E =======================================


sub_97D:
		call	sub_978
		ld	ix,zTracksSFXStart
		ld	b,SFX_TRACK_COUNT

loc_986:
		push	bc
		bit	7,(ix+zTrack.PlaybackControl)
		jp	z,loc_9EC
		res	7,(ix+zTrack.PlaybackControl)
		ld	a,(ix+zTrack.VoiceControl)
		or	a
		jp	m,loc_9BF
		push	af
		call	DoNoteOff
		pop	af
		push	ix
		sub	2
		add	a,a
		add	a,BGMChnPtrs&0FFh
		ld	(loc_9A8+2),a

loc_9A8:
		ld	ix,(BGMChnPtrs)
		res	2,(ix+zTrack.PlaybackControl)
		set	1,(ix+zTrack.PlaybackControl)
		ld	a,(ix+zTrack.VoiceIndex)
		call	zSetVoiceMusic
		pop	ix
		jp	loc_9EC
; ---------------------------------------------------------------------------

loc_9BF:
		push	af
		call	PSGNoteOff
		pop	af
		push	ix
		rra
		rra
		rra
		rra
		and	0Fh
		add	a,BGMChnPtrs&0FFh
		ld	(loc_9D1+2),a

loc_9D1:
		ld	ix,(BGMChnPtrs)
		res	2,(ix+zTrack.PlaybackControl)
		set	1,(ix+zTrack.PlaybackControl)
		ld	a,(ix+zTrack.VoiceControl)
		cp	0E0h
		jr	nz,loc_9EA
		ld	a,(ix+zTrack.PSGNoise)
		ld	(zPSG),a

loc_9EA:
		pop	ix

loc_9EC:
		ld	de,zTrack.len
		add	ix,de
		pop	bc
		dec	b
		djnz	loc_986
		ret
; End of function sub_97D

; ---------------------------------------------------------------------------

FadeOutMusic:
		call	sub_97D
		ld	a,3
		ld	(zAbsVar.FadeOutDelay),a
		ld	a,28h
		ld	(zAbsVar.FadeOutCounter),a
		xor	a
		ld	(zSongDAC),a	; 1B97 - Music Track DAC
		ld	(zAbsVar.SpeedUpFlag),a
		ret

; =============== S U B	R O U T	I N E =======================================


DoFadeOut:
		ld	a,(zAbsVar.FadeOutDelay)	; 1B85 - Fade Out Timeout Counter
		or	a
		jr	z,ApplyFadeOut	; reached 0 - apply fading
		dec	(ix+zVar.FadeOutDelay)		; decrease else
		ret
; ---------------------------------------------------------------------------

ApplyFadeOut:
		dec	(ix+zVar.FadeOutCounter)		; decrement remaining Fade Out Steps (1B84)
		jp	z,StopAllSound
		ld	(ix+zVar.FadeOutDelay),3	; reset	Fade Timeout
		push	ix
		ld	ix,zSongFMStart	; 1BC1 - Music Track FM	1
		ld	b,MUSIC_FM_TRACK_COUNT

loc_A27:
		bit	7,(ix+zTrack.PlaybackControl)
		jr	z,loc_A3E
		inc	(ix+zTrack.Volume)
		jp	p,loc_A39
		res	7,(ix+zTrack.PlaybackControl)
		jr	loc_A3E
; ---------------------------------------------------------------------------

loc_A39:
		push	bc
		call	RefreshVolume
		pop	bc

loc_A3E:
		ld	de,zTrack.len
		add	ix,de
		djnz	loc_A27
		ld	b,MUSIC_PSG_TRACK_COUNT

loc_A47:
		bit	7,(ix+zTrack.PlaybackControl)
		jr	z,loc_A66
		inc	(ix+zTrack.Volume)
		ld	a,10h
		cp	(ix+zTrack.Volume)
		jp	nc,loc_A5E
		res	7,(ix+zTrack.PlaybackControl)
		jr	loc_A66
; ---------------------------------------------------------------------------

loc_A5E:
		push	bc
		ld	b,(ix+zTrack.Volume)
		call	zPSGUpdateVol
		pop	bc

loc_A66:
		ld	de,zTrack.len
		add	ix,de
		djnz	loc_A47
		pop	ix
		ret
; End of function DoFadeOut


; =============== S U B	R O U T	I N E =======================================


SilenceFM:
		ld	a,28h		; Reg 028 - Key	Off
		ld	b,3		; loop 3 times

loc_A74:
		ld	c,b
		dec	c
		rst	zWriteFMI	; write	FM 1-3 off
		set	2,c
		rst	zWriteFMI	; write	FM 4-6 off
		djnz	loc_A74
		ld	a,30h		; start	with Reg 30
		ld	c,0FFh		; set all values to FF
		ld	b,60h		; loop over 60h	registers (30..8F)

loc_A82:
		rst	zWriteFMI	; write	Reg 0xx,Data FF
		rst	zWriteFMII	; write	Reg 1xx,Data FF
		inc	a
		djnz	loc_A82
		ret
; End of function SilenceFM


; =============== S U B	R O U T	I N E =======================================


StopAllSound:
		ld	a,2Bh
		ld	c,80h
		rst	zWriteFMI
		ld	a,c
		ld	(zAbsVar.DACEnabled),a
		ld	a,27h
		ld	c,0
		rst	zWriteFMI
		ld	hl,zAbsVar.SFXPriorityVal
		ld	de,zAbsVar.TempoTimeout
		ld	(hl),0
		ld	bc,(zTracksSFXEnd-zAbsVar)-1
		ldir
		ld	a,80h
		ld	(zAbsVar.QueueToPlay),a
		call	SilenceFM
		jp	SilencePSG
; End of function StopAllSound


; =============== S U B	R O U T	I N E =======================================


sub_AAE:
		ld	ix,zAbsVar
		ld	b,(ix+zVar.SFXPriorityVal)
		ld	c,(ix+zVar.1upPlaying)
		push	bc
		ld	b,(ix+zVar.SpeedUpFlag)
		ld	c,(ix+zVar.FadeInCounter)
		push	bc
		ld	b,(ix+zVar.Queue0)
		ld	c,(ix+zVar.Queue1)
	if FixDriverBugs
		push	bc
		ld	(ix+zVar.Queue2),b
	endif
		push	bc
		ld	hl,zAbsVar
		ld	de,zAbsVar+1
		ld	(hl),0
		ld	bc,(zTracksSongEnd-zAbsVar)-1
		ldir
		pop	bc
		ld	(ix+zVar.Queue0),b
		ld	(ix+zVar.Queue1),c
	if FixDriverBugs
		pop	bc
		ld	(ix+zVar.Queue2),b
	endif
		pop	bc
		ld	(ix+zVar.SpeedUpFlag),b
		ld	(ix+zVar.FadeInCounter),c
		pop	bc
		ld	(ix+zVar.SFXPriorityVal),b
		ld	(ix+zVar.1upPlaying),c
		ld	a,80h
		ld	(zAbsVar.QueueToPlay),a
		call	SilenceFM
		jp	SilencePSG
; End of function sub_AAE


; =============== S U B	R O U T	I N E =======================================


DoTempoDelay:
		ld	a,(zAbsVar.CurrentTempo)	; load initial Tempo (1B82)
		ld	(zAbsVar.TempoTimeout),a
		ld	hl,zTracksSongStart+zTrack.DurationTimeout	; 1B97 (DAC Track) + 0B	(Note Timeout)
		ld	de,zTrack.len
		ld	b,MUSIC_TRACK_COUNT	; 10 Music Tracks

loc_B02:
		inc	(hl)		; delay	by 1 frame
		add	hl,de		; next track
		djnz	loc_B02
		ret
; End of function DoTempoDelay

; ---------------------------------------------------------------------------

SpeedUpMusic:
		ld	b,80h
		ld	a,(zAbsVar.1upPlaying)
		or	a
		ld	a,(zAbsVar.TempoTurbo)
		jr	z,loc_B21
		jr	loc_B2C
; ---------------------------------------------------------------------------

SlowDownMusic:
		ld	b,0
		ld	a,(zAbsVar.1upPlaying)
		or	a
		ld	a,(zAbsVar.TempoMod)
		jr	z,loc_B21
		jr	loc_B2C
; ---------------------------------------------------------------------------

loc_B21:
		ld	(zAbsVar.CurrentTempo),a
		ld	(zAbsVar.TempoTimeout),a
		ld	a,b
		ld	(zAbsVar.SpeedUpFlag),a
		ret
; ---------------------------------------------------------------------------

loc_B2C:
		ld	(zSaveVar.CurrentTempo),a
		ld	(zSaveVar.TempoTimeout),a
		ld	a,b
		ld	(zSaveVar.SpeedUpFlag),a
		ret

; =============== S U B	R O U T	I N E =======================================


DoFadeIn:
		ld	a,(zAbsVar.FadeInDelay)	; 1B8F - Fade Out Timeout Counter
		or	a
		jr	z,loc_B41	; reached 0 - apply fading
		dec	(ix+zVar.FadeInDelay)	; decrease else
		ret
; ---------------------------------------------------------------------------

loc_B41:
		ld	a,(zAbsVar.FadeInCounter)	; 1B90 - remaining Fade	In Steps
		or	a
		jr	nz,ApplyFadeIn
		ld	a,(zSongDAC.PlaybackControl)
		and	0FBh		; remove 'is overridden' bit from DAC track
		ld	(zSongDAC.PlaybackControl),a
		xor	a
		ld	(zAbsVar.FadeInFlag),a	; disable Fade In
		ret
; ---------------------------------------------------------------------------

ApplyFadeIn:
		dec	(ix+zVar.FadeInCounter)	; decrement remaining Fade In Steps (1B90)
		ld	(ix+zVar.FadeInDelay),2	; reset	Fade Timeout
		push	ix
		ld	ix,zSongFMStart	; 1BC1 - Music Track FM	1
		ld	b,MUSIC_FM_TRACK_COUNT

loc_B63:
		bit	7,(ix+zTrack.PlaybackControl)
		jr	z,loc_B71
		dec	(ix+zTrack.Volume)
		push	bc
		call	RefreshVolume
		pop	bc

loc_B71:
		ld	de,zTrack.len
		add	ix,de
		djnz	loc_B63
		ld	b,MUSIC_PSG_TRACK_COUNT

loc_B7A:
		bit	7,(ix+zTrack.PlaybackControl)
		jr	z,loc_B92
		dec	(ix+zTrack.Volume)
		ld	a,(ix+zTrack.Volume)
		cp	10h
		jr	c,loc_B8C
		ld	a,0Fh

loc_B8C:
		push	bc
		ld	b,a
		call	zPSGUpdateVol
		pop	bc

loc_B92:
		ld	de,zTrack.len
		add	ix,de
		djnz	loc_B7A
		pop	ix
		ret
; End of function DoFadeIn

; ---------------------------------------------------------------------------

DoNoteOn:
		ld	a,(ix+zTrack.PlaybackControl)
		and	6
		ret	nz
		ld	a,(ix+zTrack.VoiceControl)
		or	0F0h
		ld	c,a
		ld	a,28h
	if OptimiseDriver
		jp	zWriteFMI
	else
		rst	zWriteFMI
		ret
	endif

; =============== S U B	R O U T	I N E =======================================


DoNoteOff:
		ld	a,(ix+zTrack.PlaybackControl)
		and	14h
		ret	nz
		ld	a,28h
		ld	c,(ix+zTrack.VoiceControl)
	if OptimiseDriver
		jp	zWriteFMI
	else
		rst	zWriteFMI
		ret
	endif
; End of function DoNoteOff


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; Performs a bank switch to where the music for the current track is at
; (there are two possible bank locations for music)

; SwitchMusBank:
zBankSwitchToMusic:
		ld	a,(zAbsVar.MusicBankNumber)	; get Music Bank
		or	a
		jr	nz,zSwitchToBank2

		bankswitch MusicPoint1
		ret
; loc_BCF:
zSwitchToBank2:
		bankswitch MusicPoint2
		ret
; End of function zBankSwitchToMusic

; ---------------------------------------------------------------------------
; cfHandler:
zCoordFlag:
		sub	0E0h
		add	a,a			; multiply by 4,skipping past padding
		add	a,a
		ld	(coordFlagLookup+1),a	; store into the instruction after coordflagLookup (self-modifying code)
		ld	a,(hl)
		inc	hl

; This is the lookup for Coordination flag routines
; loc_BE8:
coordFlagLookup:
		jr	$
; ---------------------------------------------------------------------------
		jp	cfE0_Pan
; ---------------------------------------------------------------------------
		nop
		jp	cfE1_Detune
; ---------------------------------------------------------------------------
		nop
		jp	cfE2_SetComm
; ---------------------------------------------------------------------------
		nop
		jp	cfE3_Return
; ---------------------------------------------------------------------------
		nop
		jp	cfE4_FadeIn
; ---------------------------------------------------------------------------
		nop
		jp	cfE5_TickMult
; ---------------------------------------------------------------------------
		nop
		jp	cfE6_ChgFMVol
; ---------------------------------------------------------------------------
		nop
		jp	cfE7_Hold
; ---------------------------------------------------------------------------
		nop
		jp	cfE8_NoteStop
; ---------------------------------------------------------------------------
		nop
		jp	cfE9_ChgTransp
; ---------------------------------------------------------------------------
		nop
		jp	cfEA_SetTempo
; ---------------------------------------------------------------------------
		nop
		jp	cfEB_TickMulAll
; ---------------------------------------------------------------------------
		nop
		jp	cfEC_ChgPSGVol
; ---------------------------------------------------------------------------
		nop
		jp	cfED_ClearPush
; ---------------------------------------------------------------------------
		nop
		jp	cfEE_null
; ---------------------------------------------------------------------------
		nop
		jp	cfEF_SetIns
; ---------------------------------------------------------------------------
		nop
		jp	cfF0_ModSetup
; ---------------------------------------------------------------------------
		nop
		jp	cfF1_ModOn
; ---------------------------------------------------------------------------
		nop
		jp	cfF2_StopTrk
; ---------------------------------------------------------------------------
		nop
		jp	cfF3_PSGNoise
; ---------------------------------------------------------------------------
		nop
		jp	cfF4_ModOff
; ---------------------------------------------------------------------------
		nop
		jp	cfF5_SetPSGIns
; ---------------------------------------------------------------------------
		nop
		jp	cfF6_GoTo
; ---------------------------------------------------------------------------
		nop
		jp	cfF7_Loop
; ---------------------------------------------------------------------------
		nop
		jp	cfF8_GoSub
; ---------------------------------------------------------------------------
		nop
		jp	cfF9_FM1Mute
; ---------------------------------------------------------------------------
		nop

cfE0_Pan:
		bit	7,(ix+zTrack.VoiceControl)
		ret	m
		bit	2,(ix+zTrack.PlaybackControl)
		ret	nz
		ld	c,a
		ld	a,(ix+zTrack.AMSFMSPan)
		and	37h
		or	c
		ld	(ix+zTrack.AMSFMSPan),a
		ld	c,a
		ld	a,(ix+zTrack.VoiceControl)
		and	3
		add	a,0B4h
	if OptimiseDriver
		jp	zWriteFMIorII
	else
		rst	zWriteFMIorII
		ret
	endif
; ---------------------------------------------------------------------------

cfE1_Detune:
		ld	(ix+zTrack.Detune),a
		ret
; ---------------------------------------------------------------------------

cfE2_SetComm:
		ld	(zAbsVar.Communication),a
		ret
; ---------------------------------------------------------------------------

cfE3_Return:
		ld	c,(ix+zTrack.StackPointer)
		ld	b,0
		push	ix
		pop	hl
		add	hl,bc
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		inc	c
		inc	c
		ld	(ix+zTrack.StackPointer),c
		ret
; ---------------------------------------------------------------------------

cfE4_FadeIn:
		ld	hl,zTracksSaveStart
		ld	de,zAbsVar
		ld	bc,zTracksSaveEnd-zTracksSaveStart
		ldir
		call	zBankSwitchToMusic
		ld	a,(zSongDAC.PlaybackControl)
		or	4		; set 'is overridden' bit on DAC track
		ld	(zSongDAC.PlaybackControl),a
		ld	a,(zAbsVar.FadeInCounter)
		ld	c,a
		ld	a,28h
		sub	c
		ld	c,a
		ld	b,MUSIC_FM_TRACK_COUNT
		ld	ix,zSongFMStart	; 1B97 - Music Track FM	1

loc_CAF:
		bit	7,(ix+zTrack.PlaybackControl)
		jr	z,loc_CCE
		set	1,(ix+zTrack.PlaybackControl)
		ld	a,(ix+zTrack.Volume)
		add	a,c
		ld	(ix+zTrack.Volume),a
		bit	2,(ix+zTrack.PlaybackControl)
		jr	nz,loc_CCE
		push	bc
		ld	a,(ix+zTrack.VoiceIndex)
		call	zSetVoiceMusic
		pop	bc

loc_CCE:
		ld	de,zTrack.len
		add	ix,de
		djnz	loc_CAF
		ld	b,3

loc_CD7:
		bit	7,(ix+zTrack.PlaybackControl)
		jr	z,loc_CEB
		set	1,(ix+zTrack.PlaybackControl)
		call	PSGNoteOff
		ld	a,(ix+zTrack.Volume)
		add	a,c
		ld	(ix+zTrack.Volume),a

loc_CEB:
		ld	de,zTrack.len
		add	ix,de
		djnz	loc_CD7
		ld	a,80h
		ld	(zAbsVar.FadeInFlag),a
		ld	a,28h
		ld	(zAbsVar.FadeInCounter),a
		xor	a
		ld	(zAbsVar.1upPlaying),a
		ld	a,(zAbsVar.DACEnabled)
		ld	c,a
		ld	a,2Bh
		rst	zWriteFMI
		pop	bc
		pop	bc
		jp	RestoreDACBank
; ---------------------------------------------------------------------------

cfE5_TickMult:
		ld	(ix+zTrack.TempoDivider),a
		ret
; ---------------------------------------------------------------------------

cfE6_ChgFMVol:
		add	a,(ix+zTrack.Volume)
		ld	(ix+zTrack.Volume),a
		jp	RefreshVolume
; ---------------------------------------------------------------------------

cfE7_Hold:
		set	4,(ix+zTrack.PlaybackControl)
		dec	hl
		ret
; ---------------------------------------------------------------------------

cfE8_NoteStop:
		ld	(ix+zTrack.NoteFillTimeout),a
		ld	(ix+zTrack.NoteFillMaster),a
		ret
; ---------------------------------------------------------------------------

cfE9_ChgTransp:
		add	a,(ix+zTrack.Transpose)
		ld	(ix+zTrack.Transpose),a
		ret
; ---------------------------------------------------------------------------

cfEA_SetTempo:
		ld	(zAbsVar.CurrentTempo),a
		ld	(zAbsVar.TempoTimeout),a
		ret
; ---------------------------------------------------------------------------

cfEB_TickMulAll:
		push	ix
		ld	ix,zTracksSongStart	; 1B97 - Music Tracks
		ld	de,zTrack.len
		ld	b,MUSIC_TRACK_COUNT

loc_D3F:
		ld	(ix+zTrack.TempoDivider),a
		add	ix,de
		djnz	loc_D3F
		pop	ix
		ret
; ---------------------------------------------------------------------------

cfEC_ChgPSGVol:
		add	a,(ix+zTrack.Volume)
		ld	(ix+zTrack.Volume),a
		ret
; ---------------------------------------------------------------------------

cfED_ClearPush:
		xor	a
		ld	(zPushingFlag),a	; clear	Pushing	Flag
		dec	hl
		ret
; ---------------------------------------------------------------------------

cfEE_null:
		dec	hl
		ret
; ---------------------------------------------------------------------------

cfEF_SetIns:
		ld	(ix+zTrack.VoiceIndex),	a
		ld	c,a
		bit	2,(ix+zTrack.PlaybackControl)
		ret	nz
		push	hl
		call	GetFMInsPtr	; also does zSetVoiceMusic
		pop	hl
		ret
; ---------------------------------------------------------------------------

GetFMInsPtr:
		ld	a,(zDoSFXFlag)	; check	Music/SFX Mode
		or	a
		ld	a,c
		jr	z,zSetVoiceMusic	; Mode 00 (Music Mode) - jump
		ld	l,(ix+zTrack.VoicePtrLow)	; load SFX track Instrument Pointer (Trk+1C/1D)
		ld	h,(ix+zTrack.VoicePtrHigh)
		jr	loc_D79
; ---------------------------------------------------------------------------

zSetVoiceMusic:
		ld	hl,(zAbsVar.VoiceTblPtr)

loc_D79:
		push	hl
		ld	c,a
		ld	b,0
		add	a,a
		ld	l,a
		ld	h,b
		add	hl,hl
		add	hl,hl
		ld	e,l
		ld	d,h
		add	hl,hl
		add	hl,de
		add	hl,bc
		pop	de
		add	hl,de

		ld	a,(hl)
		inc	hl
		ld	(loc_DBA+1),a
		ld	c,a
		ld	a,(ix+zTrack.VoiceControl)
		and	3
		add	a,0B0h
		rst	zWriteFMIorII

		sub	80h
		ld	b,4

loc_D9B:
		ld	c,(hl)
		inc	hl
		rst	zWriteFMIorII
		add	a,4
		djnz	loc_D9B
		push	af
		add	a,10h
		ld	b,10h

loc_DA7:
		ld	c,(hl)
		inc	hl
		rst	zWriteFMIorII
		add	a,4
		djnz	loc_DA7
		add	a,24h
		ld	c,(ix+zTrack.AMSFMSPan)
		rst	zWriteFMIorII
		ld	(ix+zTrack.TLPtrLow),l
		ld	(ix+zTrack.TLPtrHigh),h

loc_DBA:
		ld	a,0
		and	7
		add	a,FMAlgo_OpMask&0FFh	; lower	byte of	0DDF
		ld	e,a
		ld	d,(FMAlgo_OpMask&0FF00h)>>8	; higher byte of 0DDF
		ld	a,(de)
		ld	(ix+zTrack.VolTLMask),a
		ld	e,a
		ld	d,(ix+zTrack.Volume)
		pop	af

; =============== S U B	R O U T	I N E =======================================


SendFMVolume:
		ld	b,4

loc_DCE:
		ld	c,(hl)
		inc	hl
		rr	e
		jr	nc,loc_DD9
		push	af
	if FixDriverBugs
		set	7,c
	endif
		ld	a,d
		add	a,c
	if FixDriverBugs
		; Prevent attenuation overflow (volume underflow)
		ld	c,a
		sbc	a,a
		or	c
	endif
		ld	c,a
		pop	af

loc_DD9:
		rst	zWriteFMIorII
		add	a,4
		djnz	loc_DCE
		ret
; End of function SendFMVolume

; ---------------------------------------------------------------------------
FMAlgo_OpMask:	db 8,8,8,8,0Ch,0Eh,0Eh,0Fh

; =============== S U B	R O U T	I N E =======================================


RefreshVolume:
		bit	7,(ix+zTrack.VoiceControl)
		ret	nz
		bit	2,(ix+zTrack.PlaybackControl)
		ret	nz
		ld	e,(ix+zTrack.VolTLMask)
		ld	a,(ix+zTrack.VoiceControl)
		and	3
		add	a,40h
		ld	d,(ix+zTrack.Volume)
		bit	7,d
		ret	nz
		push	hl
		ld	l,(ix+zTrack.TLPtrLow)
		ld	h,(ix+zTrack.TLPtrHigh)
		call	SendFMVolume
		pop	hl
		ret
; End of function RefreshVolume

; ---------------------------------------------------------------------------

cfF0_ModSetup:
		set	3,(ix+zTrack.PlaybackControl)
		dec	hl
		ld	(ix+zTrack.ModulationPtrLow),l
		ld	(ix+zTrack.ModulationPtrHigh),h

loc_E18:
		ld	a,ixl
		add	a,zTrack.ModulationWait
		ld	e,a
		adc	a,ixh
		sub	e
		ld	d,a
		ldi
		ldi
		ldi
		ld	a,(hl)
		inc	hl
		srl	a
		ld	(ix+zTrack.ModulationSteps),a
		xor	a
		ld	(ix+zTrack.ModulationValLow),a
		ld	(ix+zTrack.ModulationValHigh),a
		ret
; ---------------------------------------------------------------------------

cfF1_ModOn:
		dec	hl
		set	3,(ix+zTrack.PlaybackControl)
		ret
; ---------------------------------------------------------------------------

cfF2_StopTrk:
		res	7,(ix+zTrack.PlaybackControl)
		res	4,(ix+zTrack.PlaybackControl)
		bit	7,(ix+zTrack.VoiceControl)
		jr	nz,loc_E56
		ld	a,(zAbsVar.DACUpdating)
		or	a
		jp	m,loc_ECE
		call	DoNoteOff
		jr	loc_E59
; ---------------------------------------------------------------------------

loc_E56:
		call	PSGNoteOff

loc_E59:
		ld	a,(zDoSFXFlag)	; check	Music/SFX Mode
		or	a
		jp	p,loc_ECD
		xor	a
		ld	(zAbsVar.SFXPriorityVal),a
		ld	a,(ix+zTrack.VoiceControl)
		or	a
		jp	m,loc_EA5
		push	ix
		sub	2
		add	a,a
		add	a,BGMChnPtrs&0FFh
		ld	(loc_E75+2),a

loc_E75:
		ld	ix,(BGMChnPtrs)
		bit	2,(ix+zTrack.PlaybackControl)
		jp	z,loc_EA0
		call	zBankSwitchToMusic
		res	2,(ix+zTrack.PlaybackControl)
		set	1,(ix+zTrack.PlaybackControl)
		ld	a,(ix+zTrack.VoiceIndex)
		call	zSetVoiceMusic
		bankswitch SoundIndex

loc_EA0:
		pop	ix
		pop	bc
		pop	bc
		ret
; ---------------------------------------------------------------------------

loc_EA5:
		push	ix
		rra
		rra
		rra
		rra
		and	0Fh
		add	a,BGMChnPtrs&0FFh
		ld	(loc_EB2+2),a

loc_EB2:
		ld	ix,(BGMChnPtrs)
		res	2,(ix+zTrack.PlaybackControl)
		set	1,(ix+zTrack.PlaybackControl)
		ld	a,(ix+zTrack.VoiceControl)
		cp	0E0h
		jr	nz,loc_ECB
		ld	a,(ix+zTrack.PSGNoise)
		ld	(zPSG),a

loc_ECB:
		pop	ix

loc_ECD:
		pop	bc

loc_ECE:
		pop	bc
		ret
; ---------------------------------------------------------------------------

cfF3_PSGNoise:
		ld	(ix+zTrack.VoiceControl),0E0h
		ld	(ix+zTrack.PSGNoise),a
		bit	2,(ix+zTrack.PlaybackControl)
		ret	nz
		ld	(zPSG),a
		ret
; ---------------------------------------------------------------------------

cfF4_ModOff:
		dec	hl
		res	3,(ix+zTrack.PlaybackControl)
		ret
; ---------------------------------------------------------------------------

cfF5_SetPSGIns:
		ld	(ix+zTrack.VoiceIndex),	a
		ret
; ---------------------------------------------------------------------------

cfF6_GoTo:
		ld	h,(hl)
		ld	l,a
		ret
; ---------------------------------------------------------------------------

cfF7_Loop:
		ld	c,(hl)
		inc	hl
		push	hl
		add	a,20h
		ld	l,a
		ld	h,0
		ld	e,ixl
		ld	d,ixh
		add	hl,de
		ld	a,(hl)
		or	a
		jr	nz,loc_EFF
		ld	(hl),c

loc_EFF:
		dec	(hl)
		pop	hl
		jr	z,loc_F08
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		ret
; ---------------------------------------------------------------------------

loc_F08:
		inc	hl
		inc	hl
		ret
; ---------------------------------------------------------------------------

cfF8_GoSub:
		ld	c,a
		ld	a,(ix+zTrack.StackPointer)
		sub	2
		ld	(ix+zTrack.StackPointer),a
		ld	b,(hl)
		inc	hl
		ex	de,hl
		add	a,ixl
		ld	l,a
		adc	a,ixh
		sub	l
		ld	h,a
		ld	(hl),e
		inc	hl
		ld	(hl),d
		ld	h,b
		ld	l,c
		ret
; ---------------------------------------------------------------------------

cfF9_FM1Mute:
		ld	a,88h
		ld	c,0Fh
		rst	zWriteFMI
		ld	a,8Ch
		ld	c,0Fh
		rst	zWriteFMI
		dec	hl
		ret
; ---------------------------------------------------------------------------
SndPriorities:	db 80h,70h,70h,70h,70h,70h,70h,70h,70h,70h,68h
		db 70h,70h,70h,60h,70h,70h,60h,70h,60h,70h,70h
		db 70h,70h,70h,70h,70h,70h,70h,70h,70h,7Fh,60h
		db 70h,70h,70h,70h,70h,70h,70h,70h,70h,70h,70h
		db 70h,70h,70h,70h,80h,80h,80h,80h,80h,80h,80h
		db 80h,80h,80h,80h,80h,80h,80h,80h,80h,90h,90h
		db 90h,90h,90h
; word_F75
zDACPtrTbl:
zDACPtr_Kick:
		dw	zmake68kPtr(DAC_Sample01)
zDACLenTbl:
		dw	DAC_Sample01_End-DAC_Sample01
zDACPtr_Snare:
		dw	zmake68kPtr(DAC_Sample02)
		dw	DAC_Sample02_End-DAC_Sample02
zDACPtr_Clap:
		dw	zmake68kPtr(DAC_Sample03)
		dw	DAC_Sample03_End-DAC_Sample03
zDACPtr_Scratch:
		dw	zmake68kPtr(DAC_Sample04)
		dw	DAC_Sample04_End-DAC_Sample04
zDACPtr_Timpani:
		dw	zmake68kPtr(DAC_Sample05)
		dw	DAC_Sample05_End-DAC_Sample05
zDACPtr_Tom:
		dw	zmake68kPtr(DAC_Sample06)
		dw	DAC_Sample06_End-DAC_Sample06

; byte_F8D
zDACMasterPlaylist:

; DAC samples IDs
offset :=	zDACPtrTbl
ptrsize :=	2+2
idstart :=	81h

dac_sample_metadata macro label,sampleRate
	if ("label"=="0")
	dw	0
	else
	db	id(label),dpcmLoopCounter(sampleRate)
	endif
    endm

		dac_sample_metadata zDACPtr_Kick,   8250	; 81h
		dac_sample_metadata zDACPtr_Snare, 24000	; 82h
		dac_sample_metadata zDACPtr_Clap,   8250	; 83h
		dac_sample_metadata zDACPtr_Scratch,19000	; 84h
		dac_sample_metadata zDACPtr_Timpani,7350	; 85h
		dac_sample_metadata zDACPtr_Tom,   13500	; 86h
		dac_sample_metadata	0						; 87h
		dac_sample_metadata zDACPtr_Timpani,9750	; 88h
		dac_sample_metadata zDACPtr_Timpani,8750	; 89h
		dac_sample_metadata zDACPtr_Timpani,7250	; 8Ah
		dac_sample_metadata zDACPtr_Timpani,7000	; 8Bh
		dac_sample_metadata zDACPtr_Tom,   13500	; 8Ch
		dac_sample_metadata zDACPtr_Tom,   11500	; 8Dh
		dac_sample_metadata zDACPtr_Tom,    9500	; 8Eh

VolEnvPtrs:	dw byte_FC3,byte_FDA,byte_FE1,byte_FF2,byte_100C,byte_FFD
		dw byte_1036,byte_1052,byte_107A,byte_108B,byte_10C9
		dw byte_10E5,byte_1165
byte_FC3:	db 0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5
		db 6,6,6,7,80h
byte_FDA:	db 0,2,4,6,8,10h,80h
byte_FE1:	db 0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,80h
byte_FF2:	db 0,0,2,3,4,4,5,5,5,6,80h
byte_FFD:	db 3,3,3,2,2,2,2,1,1,1,0,0,0,0,80h
byte_100C:	db 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1
		db 1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3
		db 3,3,3,3,4,80h
byte_1036:	db 0,0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,3,3
		db 3,4,4,4,5,5,5,6,7,80h
byte_1052:	db 0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,2,3,3
		db 3,3,3,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6
		db 7,7,7,80h
byte_107A:	db 0,1,2,3,4,5,6,7,8,9,0Ah,0Bh,0Ch,0Dh,0Eh
		db 0Fh,	80h
byte_108B:	db 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1
		db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
		db 1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3
		db 3,3,3,3,3,3,4,80h
byte_10C9:	db 4,4,4,3,3,3,2,2,2,1,1,1,1,1,1,1,2,2
		db 2,2,2,3,3,3,3,3,4,80h
byte_10E5:	db 4,4,3,3,2,2,1,1,1,1,1,1,1,1,1,1,1,1
		db 1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2
		db 2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3
		db 3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4
		db 4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5
		db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6
		db 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
		db 7,80h
byte_1165:	db 0,1,3,80h

; zbyte_116A:
zMasterPlaylist:

; Music IDs
; bank         - Which bank that the song is in.
; label        - The location of the song data's pointer.
music_metadata macro bank,label
    if bank
.base = MusicPoint2
    else
.base = MusicPoint1
    endif
	db	(bank<<7)|((label-.base)/2)
    endm

zMusIDPtr_OOZ:		music_metadata	1,MusPtr_OOZ
zMusIDPtr_GHZ:		music_metadata	1,MusPtr_GHZ
zMusIDPtr_MTZ:		music_metadata	1,MusPtr_MTZ
zMusIDPtr_CNZ:		music_metadata	1,MusPtr_CNZ
zMusIDPtr_DHZ:		music_metadata	1,MusPtr_DHZ
zMusIDPtr_HPZ:		music_metadata	1,MusPtr_HPZ
zMusIDPtr_NGHZ:		music_metadata	1,MusPtr_NGHZ
zMusIDPtr_DEZ:		music_metadata	1,MusPtr_DEZ
zMusIDPtr_SpecStg:	music_metadata	1,MusPtr_SpecStg
zMusIDPtr_LevelSel:	music_metadata	1,MusPtr_LevelSel
zMusIDPtr_LevelSelDup:	music_metadata	1,MusPtr_LevelSelDup
zMusIDPtr_FinalBoss:	music_metadata	1,MusPtr_FinalBoss
zMusIDPtr_CPZ:		music_metadata	1,MusPtr_CPZ
zMusIDPtr_Boss:		music_metadata	1,MusPtr_Boss
zMusIDPtr_RWZ:		music_metadata	1,MusPtr_RWZ
zMusIDPtr_SSZ:		music_metadata	1,MusPtr_SSZ
zMusIDPtr_SSZDup:	music_metadata	1,MusPtr_SSZ
zMusIDPtr_Unused1:	music_metadata	1,MusPtr_Unused1
zMusIDPtr_BOZ:		music_metadata	1,MusPtr_BOZ
zMusIDPtr_Unused2:	music_metadata	1,MusPtr_Unused2
zMusIDPtr_Invinc:	music_metadata	1,MusPtr_Invinc
zMusIDPtr_HTZ:		music_metadata	1,MusPtr_HTZ
zMusIDPtr_HTZDup:	music_metadata	1,MusPtr_HTZ
zMusIDPtr_ExtraLife:	music_metadata	0,MusPtr_ExtraLife
zMusIDPtr_Title:	music_metadata	0,MusPtr_Title
zMusIDPtr_ActClear:	music_metadata	0,MusPtr_ActClear
zMusIDPtr_GameOver:	music_metadata	0,MusPtr_GameOver
zMusIDPtr_Continue:	music_metadata	0,MusPtr_Continue
zMusIDPtr_Emerald:	music_metadata	0,MusPtr_Emerald
zMusIDPtr_EmeraldDup:	music_metadata	0,MusPtr_Emerald
zMusIDPtr_EmeraldDup2:	music_metadata	0,MusPtr_Emerald
zMusIDPtr__End:

SpeedUpTempoLst:
		db 07h
		db 72h
		db 73h
		db 26h
		db 15h
		db 08h
		db 0FFh
		db 05h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
		db 20h
zCurDAC:	db 0
byte_11A9:	db 0
zDoSFXFlag:	db 0
byte_11AB:	db 0
zPushingFlag:	db 0