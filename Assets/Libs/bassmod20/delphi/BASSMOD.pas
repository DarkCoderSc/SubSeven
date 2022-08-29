{
  BASSMOD 2.0 (c) 1999-2004 Ian Luck.
  Please report bugs/suggestions/etc... to bassmod@un4seen.com

  See the BASSMOD.CHM file for more complete documentation
}
unit Bassmod;

interface

uses
  Windows;

const
  // Error codes returned by BASSMOD_GetErrorCode()
  BASS_OK                 = 0;    // all is OK
  BASS_ERROR_MEM          = 1;    // memory error
  BASS_ERROR_FILEOPEN     = 2;    // can't open the file
  BASS_ERROR_DRIVER       = 3;    // can't find a free sound driver
  BASS_ERROR_HANDLE       = 5;    // invalid handle
  BASS_ERROR_FORMAT       = 6;    // unsupported format
  BASS_ERROR_POSITION     = 7;    // invalid playback position
  BASS_ERROR_INIT         = 8;    // BASS_Init has not been successfully called
  BASS_ERROR_ALREADY      = 14;   // already initialized/loaded
  BASS_ERROR_ILLTYPE      = 19;   // an illegal type was specified
  BASS_ERROR_ILLPARAM     = 20;   // an illegal parameter was specified
  BASS_ERROR_DEVICE       = 23;   // illegal device number
  BASS_ERROR_NOPLAY       = 24;   // not playing
  BASS_ERROR_NOMUSIC      = 28;   // no MOD music has been loaded
  BASS_ERROR_NOSYNC       = 30;   // synchronizers have been disabled
  BASS_ERROR_NOTAVAIL     = 37;   // requested data is not available
  BASS_ERROR_DECODE       = 38;   // the channel is a "decoding channel"
  BASS_ERROR_FILEFORM     = 41;   // unsupported file format
  BASS_ERROR_UNKNOWN      = -1;   // some other mystery error

  // Device setup flags
  BASS_DEVICE_8BITS       = 1;    // use 8 bit resolution, else 16 bit
  BASS_DEVICE_MONO        = 2;    // use mono, else stereo
  BASS_DEVICE_NOSYNC      = 16;   // disable synchronizers

  // Music flags
  BASS_MUSIC_RAMP         = 1;   // normal ramping
  BASS_MUSIC_RAMPS        = 2;   // sensitive ramping
  BASS_MUSIC_LOOP         = 4;      // loop music
  BASS_MUSIC_FT2MOD       = 16;     // play .MOD as FastTracker 2 does
  BASS_MUSIC_PT1MOD       = 32;     // play .MOD as ProTracker 1 does
  BASS_MUSIC_POSRESET     = 256;    // stop all notes when moving position
  BASS_MUSIC_SURROUND	  = 512;    // surround sound
  BASS_MUSIC_SURROUND2	  = 1024;   // surround sound (mode 2)
  BASS_MUSIC_STOPBACK	  = 2048;   // stop the music on a backwards jump effect
  BASS_MUSIC_CALCLEN      = 8192;   // calculate playback length
  BASS_MUSIC_NONINTER     = 16384;  // non-interpolated mixing
  BASS_MUSIC_NOSAMPLE     = $400000;// don't load the samples

  BASS_UNICODE            = $80000000;

  {
    Sync types (with BASS_ChannelSetSync() "param" and
    SYNCPROC "data" definitions) & flags.
  }
  BASS_SYNC_POS                     = 0;
  BASS_SYNC_MUSICPOS                = 0;
  {
    Sync when a music reaches a position.
    param: LOWORD=order (0=first, -1=all) HIWORD=row (0=first, -1=all)
    data : LOWORD=order HIWORD=row
  }
  BASS_SYNC_MUSICINST               = 1;
  {
    Sync when an instrument (sample for the non-instrument
    based formats) is played in a music (not including
    retrigs).
    param: LOWORD=instrument (1=first) HIWORD=note (0=c0...119=b9, -1=all)
    data : LOWORD=note HIWORD=volume (0-64)
  }
  BASS_SYNC_END                     = 2;
  {
    Sync when a music reaches the end.
    param: not used
    data : not used
  }
  BASS_SYNC_MUSICFX                 = 3;
  {
    Sync when the "sync" effect (XM/MTM/MOD: E8x/Wxx, IT/S3M: S2x) is used.
    param: 0:data=pos, 1:data="x" value
    data : param=0: LOWORD=order HIWORD=row, param=1: "x" value
  }
  BASS_SYNC_ONETIME                 = $80000000;
  { FLAG: sync only once, else continuously }

  // BASSMOD_MusicIsActive return values
  BASS_ACTIVE_STOPPED = 0;
  BASS_ACTIVE_PLAYING = 1;
  BASS_ACTIVE_PAUSED  = 3;

type
  DWORD = cardinal;
  BOOL = LongBool;
  FLOAT = Single;

  HSYNC = DWORD;

  // callback function types
  SYNCPROC = procedure(handle: HSYNC; data, user: DWORD); stdcall;

const
  bassmoddll = 'bassmod.dll';

function BASSMOD_GetVersion: DWORD; stdcall; external bassmoddll;
function BASSMOD_ErrorGetCode: DWORD; stdcall; external bassmoddll;
function BASSMOD_GetDeviceDescription(devnum: Integer): PChar; stdcall; external bassmoddll;
function BASSMOD_Init(device: Integer; freq, flags: DWORD): BOOL; stdcall; external bassmoddll;
procedure BASSMOD_Free; stdcall; external bassmoddll;
function BASSMOD_GetCPU: Single; stdcall; external bassmoddll;
function BASSMOD_SetVolume(volume: DWORD): BOOL; stdcall; external bassmoddll;
function BASSMOD_GetVolume: Integer; stdcall; external bassmoddll;

function BASSMOD_MusicLoad(mem: BOOL; f: Pointer; offset, length, flags: DWORD): BOOL; stdcall; external bassmoddll;
procedure BASSMOD_MusicFree; stdcall; external bassmoddll;
function BASSMOD_MusicGetName: PChar; stdcall; external bassmoddll;
function BASSMOD_MusicGetLength(playlen: BOOL): DWORD; stdcall; external bassmoddll;
function BASSMOD_MusicPlay: BOOL; stdcall; external bassmoddll;
function BASSMOD_MusicPlayEx(pos: DWORD; flags: Integer; reset: BOOL): BOOL; stdcall; external bassmoddll;
function BASSMOD_MusicDecode(buffer: Pointer; length: DWORD): DWORD; stdcall; external bassmoddll;
function BASSMOD_MusicSetAmplify(amp: DWORD): BOOL; stdcall; external bassmoddll;
function BASSMOD_MusicSetPanSep(pan: DWORD): BOOL; stdcall; external bassmoddll;
function BASSMOD_MusicSetPositionScaler(scale: DWORD): BOOL; stdcall; external bassmoddll;
function BASSMOD_MusicSetVolume(chanins, volume: DWORD): BOOL; stdcall; external bassmoddll;
function BASSMOD_MusicGetVolume(chanins: DWORD): DWORD; stdcall; external bassmoddll;

function BASSMOD_MusicIsActive: DWORD; stdcall; external bassmoddll;
function BASSMOD_MusicStop: BOOL; stdcall; external bassmoddll;
function BASSMOD_MusicPause: BOOL; stdcall; external bassmoddll;
function BASSMOD_MusicSetPosition(pos: DWORD): BOOL; stdcall; external bassmoddll;
function BASSMOD_MusicGetPosition: DWORD; stdcall; external bassmoddll;
function BASSMOD_MusicSetSync(atype, param: DWORD; proc: SYNCPROC; user: DWORD): HSYNC; stdcall; external bassmoddll;
function BASSMOD_MusicRemoveSync(sync: HSYNC): BOOL; stdcall; external bassmoddll;

implementation

end.
