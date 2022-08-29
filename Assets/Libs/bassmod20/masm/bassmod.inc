;  BASSMOD 2.0 (c) 1999-2002 Ian Luck.
;  Please report bugs/suggestions/etc... to bassmod@un4seen.com

;  See the BASSMOD.CHM file for more complete documentation

; Error codes returned by BASSMOD_GetErrorCode()
BASS_OK                 equ 0   ; all is OK
BASS_ERROR_MEM          equ 1   ; memory error
BASS_ERROR_FILEOPEN     equ 2   ; can't open the file
BASS_ERROR_DRIVER       equ 3   ; can't find a free/valid driver
BASS_ERROR_HANDLE       equ 5   ; invalid handle
BASS_ERROR_FORMAT       equ 6   ; unsupported format
BASS_ERROR_POSITION     equ 7   ; invalid playback position
BASS_ERROR_INIT         equ 8   ; BASS_Init has not been successfully called
BASS_ERROR_ALREADY      equ 14  ; already initialized/loaded
BASS_ERROR_ILLTYPE      equ 19  ; an illegal type was specified
BASS_ERROR_ILLPARAM     equ 20  ; an illegal parameter was specified
BASS_ERROR_DEVICE       equ 23  ; illegal device number
BASS_ERROR_NOPLAY       equ 24  ; not playing
BASS_ERROR_NOMUSIC      equ 28  ; no MOD music has been loaded
BASS_ERROR_NOSYNC       equ 30  ; synchronizers have been disabled
BASS_ERROR_NOTAVAIL     equ 37  ; requested data is not available
BASS_ERROR_DECODE       equ 38  ; the channel is a "decoding channel"
BASS_ERROR_FILEFORM     equ 41  ; unsupported file format
BASS_ERROR_UNKNOWN      equ -1  ; some other mystery error

; Device setup flags
BASS_DEVICE_8BITS       equ 1   ; use 8 bit resolution, else 16 bit
BASS_DEVICE_MONO        equ 2   ; use mono, else stereo
BASS_DEVICE_NOSYNC      equ 16  ; disable synchronizers

BASS_MUSIC_RAMP         equ     1          ; normal ramping
BASS_MUSIC_RAMPS        equ     2          ; sensitive ramping
BASS_MUSIC_LOOP         equ     4          ; loop music
BASS_MUSIC_FT2MOD       equ    16          ; play .MOD as FastTracker 2 does
BASS_MUSIC_PT1MOD       equ    32          ; play .MOD as ProTracker 1 does
BASS_MUSIC_POSRESET     equ   256          ; stop all notes when moving position
BASS_MUSIC_SURROUND     equ   512          ; surround sound
BASS_MUSIC_SURROUND2    equ  1024          ; surround sound (mode 2)
BASS_MUSIC_STOPBACK     equ  2048          ; stop the music on a backwards jump effect
BASS_MUSIC_CALCLEN      equ  8192          ; calculate playback length
BASS_MUSIC_NONINTER     equ 16384          ; non-interpolated mixing
BASS_MUSIC_NOSAMPLE     equ  0x400000      ; don't load the samples

BASS_UNICODE            equ 0x80000000

BASS_SYNC_MUSICPOS      equ 0
BASS_SYNC_POS           equ 0
BASS_SYNC_MUSICINST     equ 1
BASS_SYNC_END           equ 2
BASS_SYNC_MUSICFX       equ 3
BASS_SYNC_ONETIME       equ 0x80000000     ; FLAG: sync only once, else continuously

; BASSMOD_ChannelIsActive return values
BASS_ACTIVE_STOPPED     equ 0
BASS_ACTIVE_PLAYING     equ 1
BASS_ACTIVE_PAUSED      equ 3

; prototypes
BASSMOD_GetVersion             PROTO
BASSMOD_ErrorGetCode           PROTO
BASSMOD_GetDeviceDescription   PROTO :DWORD
BASSMOD_Init                   PROTO :DWORD,:DWORD,:DWORD
BASSMOD_Free                   PROTO
BASSMOD_GetCPU                 PROTO
BASSMOD_SetVolume              PROTO :DWORD
BASSMOD_GetVolume              PROTO
BASSMOD_MusicLoad              PROTO :DWORD,:DWORD,:DWORD,:DWORD,:DWORD
BASSMOD_MusicFree              PROTO
BASSMOD_MusicGetName           PROTO
BASSMOD_MusicGetLength         PROTO :DWORD
BASSMOD_MusicPlay              PROTO
BASSMOD_MusicPlayEx            PROTO :DWORD,:DWORD,:DWORD
BASSMOD_MusicDecode            PROTO :DWORD,:DWORD
BASSMOD_MusicSetAmplify        PROTO :DWORD
BASSMOD_MusicSetPanSep         PROTO :DWORD
BASSMOD_MusicSetPositionScaler PROTO :DWORD
BASSMOD_MusicSetVolume         PROTO :DWORD,:DWORD
BASSMOD_MusicGetVolume         PROTO :DWORD
BASSMOD_MusicIsActive          PROTO
BASSMOD_MusicStop              PROTO
BASSMOD_MusicPause             PROTO
BASSMOD_MusicSetPosition       PROTO :DWORD
BASSMOD_MusicGetPosition       PROTO
BASSMOD_MusicSetSync           PROTO :DWORD,:DWORD,:DWORD,:DWORD
BASSMOD_MusicRemoveSync        PROTO :DWORD
