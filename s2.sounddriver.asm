; Sonic the Hedgehog 2 Simon Wai disassembled Z80 sound driver

; Disassembled by ValleyBell
; Rewritten by Filter for AS

; Technically speaking, this sound driver is basically a direct port of Sonic 1's sound driver
; from 68K to Z80. Even the push block is still fully intact here.
; ---------------------------------------------------------------------------

FixDriverBugs = FixBugs

; If 0, no optimisations are made, resulting in a driver size of exactly 11AD bytes.
; If 1, size optimisations are made, resulting in a driver size of approximately 109A bytes.
; If 2, speed optimisations are made, resulting in a driver size of approximately 1179 bytes.
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

; function to decide whether an offset's full range won't fit in one byte
offsetover1byte function from,maxsize, ((from&0FFh)>(100h-maxsize))

; macro to make sure that ($ & 0FF00h) == (($+maxsize) & 0FF00h)
ensure1byteoffset macro maxsize
	if offsetover1byte($,maxsize)
startpad := $
		align 100h
		if MOMPASS=1
endpad := $
		if endpad-startpad>=1h
			; warn because otherwise you'd have no clue why you're running out of space so fast
			warning "had to insert \{endpad-startpad}h   bytes of padding before improperly located data at 0\{startpad}h in Z80 code"
		endif
		endif
	endif
	endm

; Function to turn a 68k address into a word the Z80 can use to access it,
; assuming the correct bank has been switched to first
zmake68kPtr function addr,zROMWindow+(addr&7FFFh)

; Function to turn a sample rate into a djnz loop counter
pcmLoopCounterBase function sampleRate,baseCycles, 1+(Z80_Clock/(sampleRate)-(baseCycles)+(13/2))/13
pcmLoopCounter function sampleRate, pcmLoopCounterBase(sampleRate,149/2) ; 149 is the number of cycles zPlaySegaSound takes to deliver two samples.
dpcmLoopCounter function sampleRate, pcmLoopCounterBase(sampleRate,303/2) ; 303 is the number of cycles zWriteToDAC takes to deliver two samples.

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Z80 'ROM' start:
; zEntryPoint:
		di	; disable interrupts
		ld	sp,zStack
		jp	zStartDAC
; ---------------------------------------------------------------------------

	if OptimiseDriver=0
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
	if OptimiseDriver=2
		jp	zWriteFMII
	else
		jr	zWriteFMII
	endif
; End of function zWriteFMIorII

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
		align 8
; zsub_18
zWriteFMI:	rsttarget
		; Write reg/data pair to part I; 'a' is register, 'c' is data
	if OptimiseDriver=0
		push	af
		rst	zWaitForYM
		pop	af
	endif
		ld	(zYM2612_A0),a
		push	af
	if OptimiseDriver=0
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
	if OptimiseDriver=0
		push	af
		rst	zWaitForYM
		pop	af
	endif
		ld	(zYM2612_A1),a
		push	af
	if OptimiseDriver=0
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
		call	zPauseMusic
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
		; According to Kabuto, the Z80 suffers a delay of approximately 3.3 cycles for each ROM access.
		; https://plutiedev.com/mirror/kabuto-hardware-notes#bus-system
		djnz	$		; 8	; Busy wait for specific amount of time in 'b'

		di			; 4	; disable interrupts (while updating DAC)
		ld	a,2Ah		; 7	; DAC port
		ld	(zYM2612_A0),a	; 13	; Set DAC port register
		ld	a,(hl)		; 7+3	; Get next DAC byte
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
		ld	a,(hl)		; 7+3	; Get next DAC byte
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
					; 303
		; 303 cycles for two samples. dpcmLoopCounter should use 303 divided by 2.
; ---------------------------------------------------------------------------
DPCMData:
		db 0,1,2,4,8,10h,20h,40h
		db 80h,-1,-2,-4,-8,-10h,-20h,-40h

	ensure1byteoffset 10h
zMusicTrackOffs:
		dw zSongFM3
		dw 0
		dw zSongFM4
		dw zSongFM5
		dw zSongPSG1
		dw zSongPSG2
		dw zSongPSG3
		dw zSongPSG3

	ensure1byteoffset 10h
zSFXTrackOffs:
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
	if OptimiseDriver=1
		jr	loc_1B3
	else
		jp	loc_1B3
	endif
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
	if OptimiseDriver=1
		jr	loc_1D8
	else
		jp	loc_1D8
	endif
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
	if OptimiseDriver=1
		jr	loc_223
	else
		jp	loc_223
	endif
; ---------------------------------------------------------------------------

loc_22F:
		push	af
		call	zFMNoteOff
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
	if OptimiseDriver=1
		ld	d,12*2			; 12 notes per octave
		ld	c,0			; Clear c (will hold octave bits)

.loop:
		sub	d			; Subtract 1 octave from the note
		jr	c,.getoctave		; If this is less than zero, we are done
		inc	c			; One octave up
		jr	.loop

.getoctave:
		add	a,d			; Add 1 octave back (so note index is positive)
		sla	c
		sla	c
		sla	c			; Multiply octave value by 8, to get final octave bits
	endif
		add	a,zFrequencies&0FFh
		ld	(loc_254+2),a
;		ld	d,a
;		adc	a,(zFrequencies&0FF00h)>>8
;		sub	d
;		ld	(.storefreq+3),a		; This is how you could store the high byte of the pointer too (unnecessary if it's in the right range)

loc_254:
		ld	de,(zFrequencies)
		ld	(ix+zTrack.FreqLow),e
	if OptimiseDriver=1
		ld	a,d
		or	c
		ld	(ix+zTrack.FreqHigh),a	; Frequency high byte  -> trackPtr + 0Eh
	else
		ld	(ix+zTrack.FreqHigh),d	; Frequency high byte  -> trackPtr + 0Eh
	endif
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
	if OptimiseDriver
		xor	a
	endif
		ld	b,(ix+zTrack.TempoDivider)

loc_26F:
	if OptimiseDriver
		add	a,c
		djnz	loc_26F
	else
		djnz	loc_278
	endif
		ld	(ix+zTrack.SavedDuration),a
		ld	(ix+zTrack.DurationTimeout),a
		ret
; ---------------------------------------------------------------------------
	if OptimiseDriver=0
loc_278:
		add	a,c
		jp	loc_26F
	endif
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
		jp	nz,zPSGNoteOff
		jp	zFMNoteOff
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
		jr	z,.waitdone
		dec	(ix+zTrack.ModulationWait)
		ret
; ---------------------------------------------------------------------------

.waitdone:
		dec	(ix+zTrack.ModulationSpeed)
		ret	nz
		ld	l,(ix+zTrack.ModulationPtrLow)
		ld	h,(ix+zTrack.ModulationPtrHigh)
		inc	hl
		ld	a,(hl)
		ld	(ix+zTrack.ModulationSpeed),a
		ld	a,(ix+zTrack.ModulationSteps)
		or	a
		jr	nz,.calcfreq
		inc	hl
		inc	hl
		ld	a,(hl)
		ld	(ix+zTrack.ModulationSteps),a
		ld	a,(ix+zTrack.ModulationDelta)
		neg
		ld	(ix+zTrack.ModulationDelta),a
		ret
; ---------------------------------------------------------------------------

.calcfreq:
		dec	(ix+zTrack.ModulationSteps)
		ld	l,(ix+zTrack.ModulationValLow)
		ld	h,(ix+zTrack.ModulationValHigh)
		; This is a 16-bit sign extension for 'bc'
	if OptimiseDriver
		ld	a,(ix+zTrack.ModulationDelta)	; Get current modulation change per step -> 'a'
		ld	c,a
		rla					; Carry contains sign of delta
		sbc	a,a				; a = 0 or -1 if carry is 0 or 1
		ld	b,a				; bc = sign extension of delta
	else
		ld	b,0
		ld	c,(ix+zTrack.ModulationDelta)	; Get current modulation change per step -> 'c'
		bit	7,c
		jp	z,.nosignextend
		ld	b,0FFh				; Sign extend if negative

.nosignextend:
	endif
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
zMakeFMFrequency function frequency,roundFloatToInteger(frequency*1024*1024*2/FM_Sample_Rate)
zMakeFMFrequenciesOctave macro octave
		; Frequencies for the base octave. The first frequency is B, the last frequency is B-flat.
		irp op, 15.39, 16.35, 17.34, 18.36, 19.45, 20.64, 21.84, 23.13, 24.51, 25.98, 27.53, 29.15
			dw zMakeFMFrequency(op)+octave*800h
		endm
	endm

    if OptimiseDriver=1
	ensure1byteoffset 18h
    else
	ensure1byteoffset 0C0h
    endif
zFrequencies:
	zMakeFMFrequenciesOctave 0
    if OptimiseDriver<>1	; We will calculate these, instead, which will save space
	zMakeFMFrequenciesOctave 1
	zMakeFMFrequenciesOctave 2
	zMakeFMFrequenciesOctave 3
	zMakeFMFrequenciesOctave 4
	zMakeFMFrequenciesOctave 5
	zMakeFMFrequenciesOctave 6
	zMakeFMFrequenciesOctave 7
    endif

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
		; This is a 16-bit sign extension of (ix+zTrack.Detune)
	if OptimiseDriver
		ld	a,(ix+zTrack.Detune)		; Get detune value
		ld	l,a
		rla					; Carry contains sign of detune
		sbc	a,a				; a = 0 or -1 if carry is 0 or 1
		ld	h,a				; hl = sign extension of detune
	else
		ld	h,0				; h = 0
		ld	l,(ix+zTrack.Detune)		; Get detune value
		bit	7,l				; Did prior value have 80h set?
		jr	z,.nosignextend			; If not, skip next step
		ld	h,0FFh				; h = FFh

.nosignextend:
	endif
		add	hl,de
		ld	c,h
		ld	a,(ix+zTrack.VoiceControl)
		and	3
		add	a,0A4h
		rst	zWriteFMIorII
		ld	c,l
		sub	4
	if OptimiseDriver=2
		jp	zWriteFMIorII
	else
		rst	zWriteFMIorII
		ret
	endif
; End of function SendFMFreq

; ---------------------------------------------------------------------------
zMakePSGFrequency function frequency,min(3FFh,roundFloatToInteger(PSG_Sample_Rate/(frequency*2)))
zMakePSGFrequencies macro
		irp op,ALLARGS
			dw zMakePSGFrequency(op)
		endm
	endm

	ensure1byteoffset 8Ch
zPSGFrequencies:
	; 6 octaves, each one begins with C and ends with B, with
	; the exception of the final octave, which ends at A-flat.
	; The last octave's final note is set to the PSG's maximum
	; frequency. This is typically used by the noise channel to
	; create a sound that is similar to a hi-hat.
	zMakePSGFrequencies  130.98,    138.78,    146.99,    155.79,    165.22,    174.78,    185.19,    196.24,    207.91,    220.63,    233.52,    247.47
	zMakePSGFrequencies  261.96,    277.56,    293.59,    311.58,    329.97,    349.56,    370.39,    392.49,    415.83,    440.39,    468.03,    494.95
	zMakePSGFrequencies  522.71,    556.51,    588.73,    621.44,    661.89,    699.12,    740.79,    782.24,    828.59,    880.79,    932.17,    989.91
	zMakePSGFrequencies 1045.42,   1107.52,   1177.47,   1242.89,   1316.00,   1398.25,   1491.47,   1575.50,   1669.55,   1747.82,   1864.34,   1962.46
	zMakePSGFrequencies 2071.49,   2193.34,   2330.42,   2485.78,   2601.40,   2796.51,   2943.69,   3107.23,   3290.01,   3495.64,   3608.40,   3857.25
	zMakePSGFrequencies 4142.98,   4302.32,   4660.85,   4863.50,   5084.56,   5326.69,   5887.39,   6214.47,   6580.02, 223721.56

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
	if OptimiseDriver=1
		jr	loc_4BE
	else
		jp	loc_4BE
	endif
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
		jp	zPSGNoteOff

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
		; This is a 16-bit sign extension of (ix+zTrack.Detune) -> 'hl'
	if OptimiseDriver
		ld	a,(ix+zTrack.Detune)	; Get detune value
		ld	l,a
		rla				; Carry contains sign of detune
		sbc	a,a			; a = 0 or -1 if carry is 0 or 1
		ld	h,a			; hl = sign extension of detune
	else
		ld	h,0
		ld	l,(ix+zTrack.Detune)	; hl = detune value (coord flag E9)
		bit	7,l			; Did prior value have 80h set?
		jr	z,.nosignextend		; If not, skip next step
		ld	h,0FFh			; sign extend negative value

.nosignextend:
	endif
		add	hl,de
		ld	a,(ix+zTrack.VoiceControl)
		cp	0E0h
		jr	nz,.notnoise
		ld	a,0C0h

.notnoise:
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
		ld	hl,zPSG_EnvTbl
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
	if FixDriverBugs=0
		cp	10h
		jr	c,.abovesilence
		ld	a,0Fh

.abovesilence:
	endif
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
	if FixDriverBugs
		ld	a,b				; 'b' -> 'a'
		cp	10h				; Did the level get pushed below silence level? (i.e. a > 0Fh)
		jr	c,.abovesilence
		ld	a,0Fh				; If so, fix it!

.abovesilence:
		or	(ix+zTrack.VoiceControl)	; Apply channel info (which PSG to set!)
		or	10h				; This bit marks it as an attenuation level assignment (along with channel info just above)
	else
		ld	a,(ix+zTrack.VoiceControl)
		or	b
		add	a,10h
	endif
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
		if OptimiseDriver=2
			jp	zPSGDoVolFX
		else
			jr	zPSGDoVolFX
		endif
	else
		dec	(ix+zTrack.VolFlutter)
		ret
	endif

; =============== S U B	R O U T	I N E =======================================


zPSGNoteOff:
		bit	2,(ix+zTrack.PlaybackControl)
		ret	nz
		ld	a,(ix+zTrack.VoiceControl)
		or	1Fh
		ld	(zPSG),a
	if FixDriverBugs
		; Without zInitMusicPlayback forcefully muting all channels, there's the
		; risk of music accidentally playing noise because it can't detect if
		; the PSG4/noise channel needs muting, on track initialisation.
		; This bug can be heard be playing the End of Level music in CNZ, whose
		; music uses the noise channel. S&K's driver contains a fix just like this.
		cp	0DFh		; Are we stopping PSG3?
		ret	nz
		ld	a,0FFh		; If so, stop noise channel while we're at it
		ld	(zPSG),a	; Stop noise channel
	endif
		ret
; End of function zPSGNoteOff

; ---------------------------------------------------------------------------

SilencePSG:
		ld	hl,zPSG
		ld	(hl),9Fh
		ld	(hl),0BFh
		ld	(hl),0DFh
		ld	(hl),0FFh
		ret

; =============== S U B	R O U T	I N E =======================================


zPauseMusic:
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

	if FixDriverBugs
		; Bug fix to fix SFX using music FM instruments when unpausing.
		ld	a,0FFh			; a = 0FFH
		ld	(zDoSFXFlag),a		; Set flag to say we are updating SFX
	endif
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
; End of function zPauseMusic


; =============== S U B	R O U T	I N E =======================================


zResumeTrack:
		bit	7,(ix+zTrack.PlaybackControl)
		jr	z,.nexttrack
		bit	2,(ix+zTrack.PlaybackControl)
		jr	nz,.nexttrack
    if OptimiseDriver=0
		; cfSetVoiceCont already does this
		ld	c,(ix+zTrack.AMSFMSPan)		; AMS/FMS/panning flags
		ld	a,(ix+zTrack.VoiceControl)	; Get voice control bits...
		and	3				; ... the FM portion of them
		add	a,0B4h				; Command to select AMS/FMS/panning register
		rst	zWriteFMIorII
    endif
		push	bc
	if FixDriverBugs
		; Bug fix to fix SFX using music FM instruments when unpausing.
		ld	c,(ix+zTrack.VoiceIndex)
		call	GetFMInsPtr
	else
		ld	a,(ix+zTrack.VoiceIndex)
		call	zSetVoiceMusic
	endif
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
	if OptimiseDriver=1
		ld	c,(ix+zVar.SFXPriorityVal)	; 1B80 - current SFX Priority
	else
		ld	a,(zAbsVar.SFXPriorityVal)	; 1B80 - current SFX Priority
		ld	c,a
	endif
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
	if OptimiseDriver=1
		ld	(ix+zVar.QueueToPlay),e
	else
		ld	a,e
		ld	(zAbsVar.QueueToPlay),a
	endif
		ret
; ---------------------------------------------------------------------------

loc_642:
		push	hl
		add	a,zSFXPriority&0FFh		; add lower byte of 0F30 (zSFXPriority)
		ld	l,a
		adc	a,(zSFXPriority&0FF00h)>>8	; higher byte of 0F30 (zSFXPriority)
		sub	l
		ld	h,a
		ld	a,(hl)
		cp	c
		jr	c,loc_653
		ld	c,a
	if OptimiseDriver=1
		ld	(ix+zVar.QueueToPlay),e
	else
		ld	a,e
		ld	(zAbsVar.QueueToPlay),a
	endif

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
		cp	MusID_Pause
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
	if FixDriverBugs
		; reset panning (don't want Sega sound playing on only one speaker)
		ld	a,0B6h		; Set Panning / AMS / FMS
		ld	c,0C0h		; default Panning / AMS / FMS settings (only stereo L/R enabled)
		rst	zWriteFMII	; Set it!
	endif

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
		ld	a,(hl)			; 7+3	; Get next PCM byte
		ld	(zYM2612_D0),a		; 13	; Send to DAC
		inc	hl			; 6	; Advance pointer
		nop				; 4
		ld	b,pcmLoopCounter(16500)	; 7	; Sega PCM pitch

loc_6C0:
		djnz	$			; 8	; Delay loop
		ld	a,(zAbsVar.QueueToPlay)	; 13	; Get next item to play
		cp	c			; 4	; Is it 80h?
		jr	nz,loc_6D8		; 12	; If not, stop Sega PCM
		ld	a,(hl)			; 7+3	; Get next PCM byte
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
						; 149
		; Two samples per 149 cycles, meaning that pcmLoopCounter should used 149 divided by 2.

loc_6D8:
		call	zBankSwitchToMusic
	if OptimiseDriver=1
		ld	c,(ix+zVar.DACEnabled)	; load DAC State
	else
		ld	a,(zAbsVar.DACEnabled)	; load DAC State
		ld	c,a
	endif
		ld	a,2Bh		; Reg 02B - DAC	Enable/Disable
	if OptimiseDriver=2
		jp	zWriteFMI
	else
		rst	zWriteFMI
		ret
	endif
; ---------------------------------------------------------------------------

zPlayMusic:
		ld	(zCurSong),a	; make a backup	of the Music ID
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

	if FixDriverBugs
		; This was in Sonic 1's driver, but this driver foolishly removed it.
		xor	a
		ld	(zAbsVar.SFXPriorityVal),a	; Clears SFX priority
	endif

		ld	de,zTracksSaveStart
		ld	hl,zAbsVar
		ld	bc,zTracksSaveEnd-zTracksSaveStart
		ldir
		ld	a,80h
		ld	(zAbsVar.1upPlaying),a
	if FixDriverBugs=0
		; This is done in the wrong place: it should have been done before
		; the variables are backed-up. Because of this, SFXPriorityVal will
		; be set back to a non-zero value when the 1-up jingle is over,
		; preventing lower-priority sounds from being able to play until a
		; high-priority sound is played.
		xor	a
		ld	(zAbsVar.SFXPriorityVal),a	; Clears SFX priority
	endif

	if OptimiseDriver=2
		jp	loc_72C
	else
		jr	loc_72C
	endif
; ---------------------------------------------------------------------------

loc_725:
		xor	a
		ld	(zAbsVar.1upPlaying),a
		ld	(zAbsVar.FadeInCounter),a

loc_72C:
		call	zInitMusicPlayback
		ld	a,(zCurSong)	; read Music ID	back
		sub	MusID__First
		ld	e,a
		ld	d,0
		ld	hl,zSpedUpTempoTable
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
		ld	hl,zROMWindow
		add	hl,de
		push	hl
		call	zBankSwitchToMusic
		pop	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		push	de
		pop	ix
		ld	e,(ix+0)
		ld	d,(ix+1)
		ld	(zAbsVar.VoiceTblPtr),de
		ld	a,(ix+5)
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
		ld	iy,zTracksSongStart	; 1B97 - Music Tracks
		ld	c,(ix+4)
	if FixDriverBugs=0
		; The bugfix in zInitMusicPlayback does this, already
		ld	de,zFMDACInitBytes		; 'de' points to zFMDACInitBytes
	endif

loc_79A:
	if OptimiseDriver
		ld	(iy+zTrack.PlaybackControl),80h
	else
		set	7,(iy+zTrack.PlaybackControl)
	endif
	if FixDriverBugs=0
		; The bugfix in zInitMusicPlayback does this, already
		ld	a,(de)				; Get current byte from zFMDACInitBytes -> 'a'
		inc	de				; will get next byte from zFMDACInitBytes next time
		ld	(iy+zTrack.VoiceControl),a			; Store this byte to "voice control" byte
	endif
		ld	(iy+zTrack.TempoDivider),c
		ld	(iy+zTrack.StackPointer),zTrack.GoSubStack
		ld	(iy+zTrack.AMSFMSPan),0C0h
		ld	(iy+zTrack.DurationTimeout),1
	if FixDriverBugs=0
		; The bugfix in zInitMusicPlayback does this, already
		push	de				; saving zFMDACInitBytes pointer
	endif
		push	bc
		ld	a,iyl
		add	a,zTrack.DataPointerLow
		ld	e,a
		adc	a,iyh
		sub	e
		ld	d,a
	if OptimiseDriver=1
		ld	bc,4
		ldir
	else
		ldi
		ldi
		ldi
		ldi
	endif
		ld	de,zTrack.len
		add	iy,de
		pop	bc
	if FixDriverBugs=0
		; The bugfix in zInitMusicPlayback does this, already
		pop	de			; restore 'de' (zFMDACInitBytes current pointer)
	endif
		djnz	loc_79A
		pop	iy
		ld	a,(ix+2)
		cp	7
		jr	nz,loc_7DB
		xor	a
	if OptimiseDriver=0
		ld	c,a
	endif
	if OptimiseDriver=2
		jp	loc_7F3
	else
		jr	loc_7F3
	endif
; ---------------------------------------------------------------------------

loc_7DB:
	if OptimiseDriver=0
		; A later call to zFMNoteOff does this, already
		ld	a,28h			; Key on/off FM register
		ld	c,6			; FM channel 6
		rst	zWriteFMI		; All operators off
	endif
	if FixDriverBugs=0
		; The added zFMSilenceChannel does this, already
		ld	a,42h			; Starting at FM Channel 6 Operator 1 Total Level register
		ld	c,0FFh			; Silence value
		ld	b,4			; Write to all four FM Channel 6 operators

		; Set all TL values to silence!
.silencefm6loop:
		rst	zWriteFMII
		add	a,4			; Next operator
		djnz	.silencefm6loop
	endif
		ld	a,0B6h
		ld	c,0C0h
		rst	zWriteFMII
		ld	a,80h
	if OptimiseDriver=0
		ld	c,a
	endif

loc_7F3:
	if OptimiseDriver
		ld	c,a
	endif
		ld	(zAbsVar.DACEnabled),a
		ld	a,2Bh
		rst	zWriteFMI

loc_7F9:
		ld	a,(ix+3)
		or	a
		jp	z,loc_845
		ld	b,a
		push	iy
		ld	iy,zSongPSG1
		ld	c,(ix+4)
	if FixDriverBugs=0
		; The bugfix in zInitMusicPlayback does this, already
		ld	de,zPSGInitBytes	; 'de' points to zPSGInitBytes
	endif

loc_80D:
	if OptimiseDriver
		ld	(iy+zTrack.PlaybackControl),80h
	else
		set	7,(iy+zTrack.PlaybackControl)
	endif
	if FixDriverBugs=0
		; The bugfix in zInitMusicPlayback does this, already
		ld	a,(de)				; Get current byte from zPSGInitBytes -> 'a'
		inc	de				; will get next byte from zPSGInitBytes next time
		ld	(iy+zTrack.VoiceControl),a	; Store this byte to "voice control" byte
	endif
		ld	(iy+zTrack.TempoDivider),c
		ld	(iy+zTrack.StackPointer),2Ah
		ld	(iy+zTrack.DurationTimeout),1
	if FixDriverBugs=0
		; The bugfix in zInitMusicPlayback does this, already
		push	de				; saving zPSGInitBytes pointer
	endif
		push	bc
		ld	a,iyl
		add	a,zTrack.DataPointerLow
		ld	e,a
		adc	a,iyh
		sub	e
		ld	d,a
	if OptimiseDriver=1
		ld	bc,4
		ldir
	else
		ldi
		ldi
		ldi
		ldi
	endif
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	(iy+zTrack.VoiceIndex),	a
		ld	de,zTrack.len
		add	iy,de
		pop	bc
	if FixDriverBugs=0
		; The bugfix in zInitMusicPlayback does this, already
		pop	de				; restore 'de' (zPSGInitBytes current pointer)
	endif
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
	if OptimiseDriver=2
		jp	loc_866
	else
		jr	loc_866
	endif
; ---------------------------------------------------------------------------

loc_860:
		rra
		rra
		rra
		rra
		and	0Fh

loc_866:
		add	a,zMusicTrackOffs&0FFh
		ld	(loc_86B+1),a

loc_86B:
		ld	hl,(zMusicTrackOffs)
	if FixDriverBugs
		set	2,(hl)
	else
		res	2,(hl)
	endif

loc_870:
		add	ix,de
		djnz	loc_84E
		ld	ix,zSongFMStart	; 1BC1 - Music Track FM	1
		ld	b,MUSIC_FM_TRACK_COUNT

loc_87A:
	if FixDriverBugs
		; zFMNoteOff isn't enough to silence the entire channel:
		; For added measure, we set Total Level and Release Rate, too.
		push	bc
		bit	2,(ix+zTrack.PlaybackControl)	; Is bit 2 (SFX overriding) set?
		call	z,zFMSilenceChannel		; If not, jump
		add	ix,de				; Next track
		pop	bc
	else
		call	zFMNoteOff		; Send Key Off
		add	ix,de			; Next track
	endif
		djnz	loc_87A
		ld	b,MUSIC_PSG_TRACK_COUNT

loc_883:
		call	zPSGNoteOff
		add	ix,de
		djnz	loc_883
		ret

	if FixDriverBugs
zFMSilenceChannel:
		call	zSetMaxRelRate
		ld	a,(ix+zTrack.VoiceControl)	; Get voice control byte
		and	3				; Channels only!
		add	a,40h				; Set total level...
		ld	c,7Fh				; ... to minimum envelope amplitude...
		call	zFMOperatorWriteLoop		; ... for all operators of this track's channel
		jp	zFMNoteOff

zSetMaxRelRate:
		ld	a,(ix+zTrack.VoiceControl)	; Get voice control byte
		and	3				; Channels only!
		add	a,80h				; Add register 80, set D1L to minimum and RR to maximum...
		ld	c,0FFh				; ... for all operators on this track's channel

zFMOperatorWriteLoop:
		ld	b,4		; Loop 4 times

.loop:
		rst	zWriteFMIorII	; Write to part I or II, as appropriate
		add	a,4		; a += 4
		djnz	.loop		; Loop
		ret
	endif
; ---------------------------------------------------------------------------
zFMDACInitBytes:
		db 6,0,1,2,4,5,6
zPSGInitBytes:
		db 80h,0A0h,0C0h
; ---------------------------------------------------------------------------

PlaySFX:
		ld	c,a
		ld	a,(ix+zVar.1upPlaying)
		or	(ix+zVar.FadeOutCounter)
		or	(ix+zVar.FadeInFlag)
		jp	nz,sub_978
		ld	a,c
		cp	SndID_RingRight
		jr	nz,loc_8B6
		ld	a,(zRingSpeaker)	; check	Ring Speaker
		or	a
		jr	nz,loc_8AF
		ld	c,SndID_RingLeft	; change SFX ID,play on left speaker

loc_8AF:
		cpl
		ld	(zRingSpeaker),a	; write	inverted Ring Speaker value back
	if OptimiseDriver=1
		jr	loc_8C5
	else
		jp	loc_8C5
	endif
; ---------------------------------------------------------------------------

loc_8B6:
	if OptimiseDriver=0
		ld	a,c
	endif
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
	if OptimiseDriver=1
		jr	loc_91A
	else
		jp	loc_91A
	endif
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
		add	a,zMusicTrackOffs&0FFh
		ld	(loc_91F+1),a

loc_91F:
		ld	hl,(zMusicTrackOffs)
		set	2,(hl)
		add	a,zSFXTrackOffs-zMusicTrackOffs
		ld	(loc_929+2),a

loc_929:
		ld	ix,(zSFXTrackOffs)
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
		ld	(ix+zTrack.StackPointer),zTrack.GoSubStack
		ld	a,e
		add	a,zTrack.DataPointerLow-zTrack.TempoDivider
		ld	e,a
		adc	a,d
		sub	e
		ld	d,a
	if OptimiseDriver=1
		ld	bc,4
		ldir
	else
		ldi
		ldi
		ldi
		ldi
	endif

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
		call	zFMNoteOff
		pop	af
		push	ix
		sub	2
		add	a,a
		add	a,zMusicTrackOffs&0FFh
		ld	(loc_9A8+2),a

loc_9A8:
		ld	ix,(zMusicTrackOffs)
	if FixDriverBugs
		bit	2,(ix+zTrack.PlaybackControl)	; Was this music track is overridden by an SFX track?
		jr	z,.notoverridden		; If not, do nothing
	endif
		res	2,(ix+zTrack.PlaybackControl)
		set	1,(ix+zTrack.PlaybackControl)
		ld	a,(ix+zTrack.VoiceIndex)
		call	zSetVoiceMusic

	if FixDriverBugs
.notoverridden:
	endif
		pop	ix
	if OptimiseDriver=1
		jr	loc_9EC
	else
		jp	loc_9EC
	endif
; ---------------------------------------------------------------------------

loc_9BF:
		push	af
		call	zPSGNoteOff
		pop	af
		push	ix
		rra
		rra
		rra
		rra
		and	0Fh
		add	a,zMusicTrackOffs&0FFh
		ld	(loc_9D1+2),a

loc_9D1:
		ld	ix,(zMusicTrackOffs)
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
	if OptimiseDriver=1
		ld	(ix+zVar.FadeOutDelay),3
		ld	(ix+zVar.FadeOutCounter),28h
	else
		ld	a,3
		ld	(zAbsVar.FadeOutDelay),a
		ld	a,28h
		ld	(zAbsVar.FadeOutCounter),a
	endif
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
	if OptimiseDriver=2
		jp	loc_A3E
	else
		jr	loc_A3E
	endif
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
	if OptimiseDriver=1
		jr	nc,loc_A5E
	else
		jp	nc,loc_A5E
	endif
		res	7,(ix+zTrack.PlaybackControl)
	if OptimiseDriver=2
		jp	loc_A66
	else
		jr	loc_A66
	endif
; ---------------------------------------------------------------------------

loc_A5E:
		push	bc
		ld	b,(ix+zTrack.Volume)
	if FixDriverBugs
		ld	a,(ix+zTrack.VoiceIndex)
		or	a				; Is this track using volume envelope 0 (no envelope)?
		call	z,zPSGUpdateVol			; If so, update volume (this code is only run on envelope 1+, so we need to do it here for envelope 0)
	else
		; DANGER! This code ignores volume envelopes, breaking fade on envelope-using tracks.
		; (It's also a part of the envelope-processing code, so calling it here is redundant)
		; This is only useful for envelope 0 (no envelope).
		call	zPSGUpdateVol			; Update volume (ignores current envelope!!!)
	endif
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
		ld	hl,zAbsVar
		ld	de,zAbsVar+1
		ld	(hl),0
		ld	bc,(zTracksSFXEnd-zAbsVar)-1
		ldir
		ld	a,80h
		ld	(zAbsVar.QueueToPlay),a
		call	SilenceFM
		jp	SilencePSG
; End of function StopAllSound


; =============== S U B	R O U T	I N E =======================================


zInitMusicPlayback:
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
	if OptimiseDriver=1
		ld	(ix+zVar.QueueToPlay),80h
	else
		ld	a,80h
		ld	(zAbsVar.QueueToPlay),a
	endif

	if FixDriverBugs
		; If a music file's header doesn't define each and every channel, they
		; won't be silenced by zSFXFinishSetup, because their tracks aren't properly
		; initialised. This can cause hanging notes. So, we'll set them up
		; properly here.
		ld	ix,zTracksSongStart			; Start at the first music track...
		ld	b,MUSIC_TRACK_COUNT		; ...and continue to the last
		ld	de,zTrack.len
		ld	hl,zFMDACInitBytes		; This continues into zPSGInitBytes

.loop:
		ld	a,(hl)
		inc	hl
		ld	(ix+zTrack.VoiceControl),a	; Set channel type while we're at it, so subroutines understand what the track is
		add	ix,de				; Next track
		djnz	.loop				; Loop for all channels

		ret
	else
		; This silences all channels, even those being used by SFX!
		; zSFXFinishSetup does the same thing, only better (it doesn't affect SFX channels)
		call	SilenceFM
		jp	SilencePSG
	endif
; End of function zInitMusicPlayback


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
	if FixDriverBugs=0
		ld	a,(ix+zTrack.Volume)
		cp	10h
		jr	c,loc_B8C
		ld	a,0Fh

loc_B8C:
	endif
		push	bc
	if FixDriverBugs
		ld	b,(ix+zTrack.Volume)		; Channel volume -> 'b'
	else
		ld	b,a
	endif
	if FixDriverBugs
		ld	a,(ix+zTrack.VoiceIndex)
		or	a				; Is this track using volume envelope 0 (no envelope)?
		call	z,zPSGUpdateVol			; If so, update volume (this code is only run on envelope 1+, so we need to do it here for envelope 0)
	else
		; DANGER! This code ignores volume envelopes, breaking fade on envelope-using tracks.
		; (It's also a part of the envelope-processing code, so calling it here is redundant)
		; This is only useful for envelope 0 (no envelope).
		call	zPSGUpdateVol	; Update volume (ignores current envelope!!!)
	endif
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
	if OptimiseDriver=2
		jp	zWriteFMI
	else
		rst	zWriteFMI
		ret
	endif

; =============== S U B	R O U T	I N E =======================================


zFMNoteOff:
		ld	a,(ix+zTrack.PlaybackControl)
		and	14h
		ret	nz
		ld	a,28h
		ld	c,(ix+zTrack.VoiceControl)
	if OptimiseDriver=2
		jp	zWriteFMI
	else
		rst	zWriteFMI
		ret
	endif
; End of function zFMNoteOff


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
	if OptimiseDriver=1
		ld	c,a	; Multiply by 3; this lets us remove padding that was
		add	a,a	; left over from Sonic 1's sound driver, which used
		add	a,c	; 4 byte-long instructions for each entry
	else
		add	a,a	; Multiply by 4, skipping past padding
		add	a,a
	endif
		ld	(coordFlagLookup+1),a	; store into the instruction after coordflagLookup (self-modifying code)
		ld	a,(hl)
		inc	hl

; This is the lookup for Coordination flag routines
; loc_BE8:
coordFlagLookup:
		jr	$
		jp	cfE0_Pan
	if OptimiseDriver<>1
		nop
	endif
		jp	cfE1_Detune
	if OptimiseDriver<>1
		nop
	endif
		jp	cfE2_SetComm
	if OptimiseDriver<>1
		nop
	endif
		jp	cfE3_Return
	if OptimiseDriver<>1
		nop
	endif
		jp	cfE4_FadeIn
	if OptimiseDriver<>1
		nop
	endif
		jp	cfE5_TickMult
	if OptimiseDriver<>1
		nop
	endif
		jp	cfE6_ChgFMVol
	if OptimiseDriver<>1
		nop
	endif
		jp	cfE7_Hold
	if OptimiseDriver<>1
		nop
	endif
		jp	cfE8_NoteStop
	if OptimiseDriver<>1
		nop
	endif
		jp	cfE9_ChgTransp
	if OptimiseDriver<>1
		nop
	endif
		jp	cfEA_SetTempo
	if OptimiseDriver<>1
		nop
	endif
		jp	cfEB_TickMulAll
	if OptimiseDriver<>1
		nop
	endif
		jp	cfEC_ChgPSGVol
	if OptimiseDriver<>1
		nop
	endif
		jp	cfED_ClearPush
	if OptimiseDriver<>1
		nop
	endif
		jp	cfEE_null
	if OptimiseDriver<>1
		nop
	endif
		jp	cfEF_SetIns
	if OptimiseDriver<>1
		nop
	endif
		jp	cfF0_ModSetup
	if OptimiseDriver<>1
		nop
	endif
		jp	cfF1_ModOn
	if OptimiseDriver<>1
		nop
	endif
		jp	cfF2_StopTrk
	if OptimiseDriver<>1
		nop
	endif
		jp	cfF3_PSGNoise
	if OptimiseDriver<>1
		nop
	endif
		jp	cfF4_ModOff
	if OptimiseDriver<>1
		nop
	endif
		jp	cfF5_SetPSGIns
	if OptimiseDriver<>1
		nop
	endif
		jp	cfF6_GoTo
	if OptimiseDriver<>1
		nop
	endif
		jp	cfF7_Loop
	if OptimiseDriver<>1
		nop
	endif
		jp	cfF8_GoSub
	if OptimiseDriver<>1
		nop
	endif
		jp	cfF9_FM1Mute
	if OptimiseDriver<>1
		nop
	endif
; ---------------------------------------------------------------------------

cfE0_Pan:
		bit	7,(ix+zTrack.VoiceControl)
		ret	m
	if FixDriverBugs=0
		; This check is in the wrong place.
		; If this flag is triggered by a music track while it's being overridden
		; by an SFX, it will use the old panning when the SFX ends.
		; This is because zTrack.AMSFMSPan doesn't get updated.
		bit	2,(ix+zTrack.PlaybackControl)	; If "SFX overriding" bit set...
		ret	nz				; return
	endif
		ld	c,a
		ld	a,(ix+zTrack.AMSFMSPan)
		and	37h
		or	c
		ld	(ix+zTrack.AMSFMSPan),a
	if FixDriverBugs
		; The check should only stop hardware access, like this.
		bit	2,(ix+zTrack.PlaybackControl)	; If "SFX overriding" bit set...
		ret	nz				; return
	endif
		ld	c,a
		ld	a,(ix+zTrack.VoiceControl)
		and	3
		add	a,0B4h
	if OptimiseDriver=2
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
		call	zPSGNoteOff
		ld	a,(ix+zTrack.Volume)
		add	a,c
		ld	(ix+zTrack.Volume),a
	if FixDriverBugs
		; Restore PSG noise type
		ld	a,(ix+zTrack.VoiceControl)
		cp	0E0h				; Is this the noise channel?
		jr	nz,loc_CEB			; If not, jump
		ld	a,(ix+zTrack.PSGNoise)
		ld	(zPSG),a			; Restore Noise setting
	endif

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
	if OptimiseDriver=0
		dec	hl
		ret
	endif
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
	if OptimiseDriver=2
		jp	loc_D79
	else
		jr	loc_D79
	endif
; ---------------------------------------------------------------------------

zSetVoiceMusic:
		ld	hl,(zAbsVar.VoiceTblPtr)

loc_D79:

	if OptimiseDriver=1
		or	a
		jr	z,.havevoiceptr
		ld	de,25

		ld	b,a

.voicemultiply:
		add	hl,de
		djnz	.voicemultiply

.havevoiceptr:
	else
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
	endif

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
	ensure1byteoffset 8
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
	if OptimiseDriver=1
		ld	bc,3
		ldir
	else
		ldi
		ldi
		ldi
	endif
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
		call	zFMNoteOff
	if OptimiseDriver=2
		jp	loc_E59
	else
		jr	loc_E59
	endif
; ---------------------------------------------------------------------------

loc_E56:
		call	zPSGNoteOff

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
		add	a,zMusicTrackOffs&0FFh
		ld	(loc_E75+2),a

loc_E75:
		ld	ix,(zMusicTrackOffs)
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
		add	a,zMusicTrackOffs&0FFh
		ld	(loc_EB2+2),a

loc_EB2:
		ld	ix,(zMusicTrackOffs)
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
		add	a,zTrack.LoopCounters
		ld	l,a
		ld	h,0
		ld	e,ixl
		ld	d,ixh
		add	hl,de
		ld	a,(hl)
		or	a
		jr	nz,.loopexists
		ld	(hl),c

.loopexists:
		dec	(hl)
		pop	hl
		jr	z,.noloop
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		ret
; ---------------------------------------------------------------------------

.noloop:
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
zSFXPriority:
		db 80h,70h,70h,70h,70h,70h,70h,70h,70h,70h,68h,70h,70h,70h,60h	; A0
		db 70h,70h,60h,70h,60h,70h,70h,70h,70h,70h,70h,70h,70h,70h,70h	; B0
		db 70h,7Fh,60h,70h,70h,70h,70h,70h,70h,70h,70h,70h,70h,70h,70h	; C0
		db 70h,70h,70h,80h,80h,80h,80h,80h,80h,80h,80h,80h,80h,80h,80h	; D0
		db 80h,80h,80h,80h,90h,90h,90h,90h,90h							; E0

dac_sample_pointer macro label
	dw	zmake68kPtr(label)
	dw	label_End-label
	endm

	ensure1byteoffset 2*0Ch
; word_F75
zDACPtrTbl:
zDACLenTbl = zDACPtrTbl + 2
zDACPtr_Kick:	dac_sample_pointer	DAC_Sample01
zDACPtr_Snare:	dac_sample_pointer	DAC_Sample02
zDACPtr_Clap:	dac_sample_pointer	DAC_Sample03
zDACPtr_Scratch:	dac_sample_pointer	DAC_Sample04
zDACPtr_Timpani:	dac_sample_pointer	DAC_Sample05
zDACPtr_Tom:	dac_sample_pointer	DAC_Sample06

	ensure1byteoffset 2*0Eh
; byte_F8D
zDACMasterPlaylist:

; DAC samples IDs
offset :=	zDACPtrTbl
ptrsize :=	2+2
idstart :=	81h

dac_sample_metadata macro label,sampleRate
	if "label"=""
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
		dac_sample_metadata							; 87h
		dac_sample_metadata zDACPtr_Timpani,9750	; 88h
		dac_sample_metadata zDACPtr_Timpani,8750	; 89h
		dac_sample_metadata zDACPtr_Timpani,7150	; 8Ah
		dac_sample_metadata zDACPtr_Timpani,7000	; 8Bh
		dac_sample_metadata zDACPtr_Tom,   13500	; 8Ch
		dac_sample_metadata zDACPtr_Tom,   11250	; 8Dh
		dac_sample_metadata zDACPtr_Tom,    9250	; 8Eh

zPSG_EnvTbl:
		dw zPSG_Env1, zPSG_Env2, zPSG_Env3
		dw zPSG_Env4, zPSG_Env5, zPSG_Env6
		dw zPSG_Env7, zPSG_Env8, zPSG_Env9
		dw zPSG_Env10, zPSG_Env11, zPSG_Env12
		dw zPSG_Env13
zPSG_Env1:	db 0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,80h
zPSG_Env2:	db 0,2,4,6,8,10h,80h
zPSG_Env3:	db 0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,80h
zPSG_Env4:	db 0,0,2,3,4,4,5,5,5,6,80h
zPSG_Env6:	db 3,3,3,2,2,2,2,1,1,1,0,0,0,0,80h
zPSG_Env5:	db 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2
		db 2,2,2,3,3,3,3,3,3,3,3,4,80h
zPSG_Env7:	db 0,0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,3,3,3,4,4,4,5,5,5,6,7,80h
zPSG_Env8:	db 0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5
		db 5,5,6,6,6,6,6,7,7,7,80h
zPSG_Env9:	db 0,1,2,3,4,5,6,7,8,9,0Ah,0Bh,0Ch,0Dh,0Eh,0Fh,80h
zPSG_Env10:	db 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
		db 1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3
		db 4,80h
zPSG_Env11:	db 4,4,4,3,3,3,2,2,2,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,80h
zPSG_Env12:	db 4,4,3,3,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2
		db 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3
		db 3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5
		db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
		db 6,6,6,6,7,80h
zPSG_Env13:	db 0,1,3,80h

; Stuff for zMasterPlaylist.
z80_bank_size = 8000h
getZ80BankOffset function label, label # z80_bank_size
getZ80BankBase function label, label - getZ80BankOffset(label)
withinSameZ80Bank function label1, label2, getZ80BankBase(label1) == getZ80BankBase(label2)

music_metadata macro DATA
	db	(withinSameZ80Bank(DATA.pointer, MusicPoint2)<<7)|(getZ80BankOffset(DATA.pointer)/2)
	endm

; zbyte_116A:
zMasterPlaylist:
zMusIDPtr_OOZ:		music_metadata	Mus_OOZ
zMusIDPtr_GHZ:		music_metadata	Mus_GHZ
zMusIDPtr_MTZ:		music_metadata	Mus_MTZ
zMusIDPtr_CNZ:		music_metadata	Mus_CNZ
zMusIDPtr_DHZ:		music_metadata	Mus_DHZ
zMusIDPtr_HPZ:		music_metadata	Mus_HPZ
zMusIDPtr_NGHZ:		music_metadata	Mus_NGHZ
zMusIDPtr_DEZ:		music_metadata	Mus_DEZ
zMusIDPtr_SpecStg:	music_metadata	Mus_SpecStg
zMusIDPtr_LevelSel:	music_metadata	Mus_LevelSel
zMusIDPtr_LevelSelDup:	music_metadata	Mus_LevelSelDup
zMusIDPtr_FinalBoss:	music_metadata	Mus_FinalBoss
zMusIDPtr_CPZ:		music_metadata	Mus_CPZ
zMusIDPtr_Boss:		music_metadata	Mus_Boss
zMusIDPtr_RWZ:		music_metadata	Mus_RWZ
zMusIDPtr_SSZ:		music_metadata	Mus_SSZ
zMusIDPtr_SSZDup:	music_metadata	Mus_SSZ
zMusIDPtr_Unused1:	music_metadata	Mus_Unused1
zMusIDPtr_BOZ:		music_metadata	Mus_BOZ
zMusIDPtr_Unused2:	music_metadata	Mus_Unused2
zMusIDPtr_Invinc:	music_metadata	Mus_Invinc
zMusIDPtr_HTZ:		music_metadata	Mus_HTZ
zMusIDPtr_HTZDup:	music_metadata	Mus_HTZ
zMusIDPtr_ExtraLife:	music_metadata	Mus_ExtraLife
zMusIDPtr_Title:	music_metadata	Mus_Title
zMusIDPtr_ActClear:	music_metadata	Mus_ActClear
zMusIDPtr_GameOver:	music_metadata	Mus_GameOver
zMusIDPtr_Continue:	music_metadata	Mus_Continue
zMusIDPtr_Emerald:	music_metadata	Mus_Emerald
zMusIDPtr_EmeraldDup:	music_metadata	Mus_Emerald
zMusIDPtr_EmeraldDup2:	music_metadata	Mus_Emerald
zMusIDPtr__End:

; The first 8 entries are identical to Sonic 1's speed up tempo list.
zSpedUpTempoTable:
		db 07h	; GHZ
		db 72h	; LZ
		db 73h	; MZ
		db 26h	; SLZ
		db 15h	; SYZ
		db 08h	; SBZ
		db 0FFh	; Invincibility
		db 05h	; Extra Life
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
; ---------------------------------------------------------------------------

; space for a few global variables
zCurDAC:	db 0	; seems to indicate DAC sample playing status
zCurSong:	db 0	; currently playing song index
zDoSFXFlag:	db 0	; flag to indicate we're updating SFX (and thus use custom voice table); set to anything but 0 while doing SFX, 0 when not.
zRingSpeaker:	db 0	; stereo alternation flag. 0 = next one plays on left, -1 = next one plays on right
zPushingFlag:	db 0