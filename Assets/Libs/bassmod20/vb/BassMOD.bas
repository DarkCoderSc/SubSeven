Attribute VB_Name = "BassMOD"
' BASSMOD 2.0 copyright (c) 1999-2004 Ian Luck.
' Please report bugs/suggestions/etc... to bassmod@un4seen.com

'See the BASSMOD.CHM file for more complete documentation

'NOTE: Use the VBStrFromAnsiPtr function to convert "char *" to VB "String".

Global Const BASSTRUE As Integer = 1   'Use this instead of VB Booleans
Global Const BASSFALSE As Integer = 0  'Use this instead of VB Booleans

' Error codes returned by BASSMOD_GetErrorCode()
Global Const BASS_OK = 0 ' all is OK
Global Const BASS_ERROR_MEM = 1 ' memory error
Global Const BASS_ERROR_FILEOPEN = 2 ' can't open the file
Global Const BASS_ERROR_DRIVER = 3 ' can't find a free/valid driver
Global Const BASS_ERROR_HANDLE = 5 ' invalid handle
Global Const BASS_ERROR_FORMAT = 6 ' unsupported format
Global Const BASS_ERROR_POSITION = 7 ' invalid playback position
Global Const BASS_ERROR_INIT = 8 ' BASS_Init has not been successfully called
Global Const BASS_ERROR_ALREADY = 14 ' already initialized/loaded
Global Const BASS_ERROR_NOPAUSE = 16 ' not paused
Global Const BASS_ERROR_ILLTYPE = 19 ' an illegal type was specified
Global Const BASS_ERROR_ILLPARAM = 20 ' an illegal parameter was specified
Global Const BASS_ERROR_DEVICE = 23 ' illegal device number
Global Const BASS_ERROR_NOPLAY = 24 ' not playing
Global Const BASS_ERROR_NOMUSIC = 28 ' no MOD music has been loaded
Global Const BASS_ERROR_NOSYNC = 30  ' synchronizers have been disabled
Global Const BASS_ERROR_NOTAVAIL = 37 ' requested data is not available
Global Const BASS_ERROR_DECODE = 38 ' the channel is a "decoding channel"
Global Const BASS_ERROR_FILEFORM = 41 ' unsupported file format
Global Const BASS_ERROR_UNKNOWN = -1 ' some other mystery error

' Device setup flags
Global Const BASS_DEVICE_8BITS = 1 ' use 8 bit resolution, else 16 bit
Global Const BASS_DEVICE_MONO = 2 ' use mono, else stereo
Global Const BASS_DEVICE_NOSYNC = 16   'disable synchronizers

' Music flags
Global Const BASS_MUSIC_RAMP = 1 ' normal ramping
Global Const BASS_MUSIC_RAMPS = 2 ' sensitive ramping
Global Const BASS_MUSIC_LOOP = 4 ' loop music
Global Const BASS_MUSIC_FT2MOD = 16 ' play .MOD as FastTracker 2 does
Global Const BASS_MUSIC_PT1MOD = 32 ' play .MOD as ProTracker 1 does
Global Const BASS_MUSIC_POSRESET = 256 ' stop all notes when moving position
Global Const BASS_MUSIC_SURROUND = 512 'surround sound
Global Const BASS_MUSIC_SURROUND2 = 1024 'surround sound (mode 2)
Global Const BASS_MUSIC_STOPBACK = 2048 'stop the music on a backwards jump effect
Global Const BASS_MUSIC_CALCLEN = 8192 'calculate playback length
Global Const BASS_MUSIC_NONINTER = 16384 ' non-interpolated mixing
Global Const BASS_MUSIC_NOSAMPLE = &H400000 ' don't load the samples

Global Const BASS_UNICODE = &H80000000

' Sync types (with BASSMOD_MusicSetSync() "param" and SYNCPROC "data"
' definitions) & flags.
Global Const BASS_SYNC_POS = 0
Global Const BASS_SYNC_MUSICPOS = 0
' Sync when the music reaches a position.
' param: LOWORD=order (0=first, -1=all) HIWORD=row (0=first, -1=all)
' data : LOWORD=order HIWORD=row
Global Const BASS_SYNC_MUSICINST = 1
' Sync when an instrument (sample for the non-instrument based formats)
' is played in the music (not including retrigs).
' param: LOWORD=instrument (1=first) HIWORD=note (0=c0...119=b9, -1=all)
' data : LOWORD=note HIWORD=volume (0-64)
Global Const BASS_SYNC_END = 2 '
' Sync when the music reaches the end.
' param: not used
' data : not used
Global Const BASS_SYNC_MUSICFX = 3
' Sync when the "sync" effect (XM/MTM/MOD: E8x/Wxx, IT/S3M: S2x) is used.
' param: 0:data=pos, 1:data="x" value
' data : param=0: LOWORD=order HIWORD=row, param=1: "x" value
Global Const BASS_SYNC_ONETIME = 2147483648# ' FLAG: sync only once, else continuously
' Sync callback function. NOTE: a sync callback function should be very
' quick (eg. just posting a message) as other syncs cannot be processed
' until it has finished.
' handle : The sync that has occured
' data   : Additional data associated with the sync's occurance

' BASSMOD_MusicIsActive return values
Global Const BASS_ACTIVE_STOPPED = 0
Global Const BASS_ACTIVE_PLAYING = 1
Global Const BASS_ACTIVE_PAUSED = 3

Declare Function BASSMOD_GetVersion Lib "bassmod.dll" () As Long
Declare Function BASSMOD_ErrorGetCode Lib "bassmod.dll" () As Long
Declare Function BASSMOD_GetDeviceDescription Lib "bassmod.dll" (ByVal devnum As Long) As Long
Declare Function BASSMOD_Init Lib "bassmod.dll" (ByVal device As Long, ByVal freq As Long, ByVal flags As Long) As Integer
Declare Sub BASSMOD_Free Lib "bassmod.dll" ()
Declare Function BASSMOD_GetCPU Lib "bassmod.dll" () As Single
Declare Function BASSMOD_SetVolume Lib "bassmod.dll" (ByVal volume As Long) As Integer
Declare Function BASSMOD_GetVolume Lib "bassmod.dll" () As Integer

Declare Function BASSMOD_MusicLoad Lib "bassmod.dll" (ByVal mem As Integer, ByVal pfile As Any, ByVal offset As Long, ByVal length As Long, ByVal flags As Long) As Integer
Declare Sub BASSMOD_MusicFree Lib "bassmod.dll" ()
Declare Function BASSMOD_MusicGetName Lib "bassmod.dll" () As String
Declare Function BASSMOD_MusicGetLength Lib "bassmod.dll" (ByVal playlen As Integer) As Long
Declare Function BASSMOD_MusicPlay Lib "bassmod.dll" () As Integer
Declare Function BASSMOD_MusicPlayEx Lib "bassmod.dll" (ByVal pos As Long, ByVal flags As Long, ByVal reset As Integer) As Integer
Declare Function BASSMOD_MusicDecode Lib "bassmod.dll" (ByRef buffer As Any, ByVal length As Long) As Long
Declare Function BASSMOD_MusicSetAmplify Lib "bassmod.dll" (ByVal amp As Long) As Integer
Declare Function BASSMOD_MusicSetPanSep Lib "bassmod.dll" (ByVal pan As Long) As Integer
Declare Function BASSMOD_MusicSetPositionScaler Lib "bassmod.dll" (ByVal pscale As Long) As Integer
Declare Function BASSMOD_MusicSetVolume Lib "bassmod.dll" (ByVal chanins As Long, ByVal volume As Long) As Long
Declare Function BASSMOD_MusicGetVolume Lib "bassmod.dll" (ByVal chanins As Long) As Long

Declare Function BASSMOD_MusicIsActive Lib "bassmod.dll" () As Long
Declare Function BASSMOD_MusicStop Lib "bassmod.dll" () As Integer
Declare Function BASSMOD_MusicPause Lib "bassmod.dll" () As Integer
Declare Function BASSMOD_MusicSetPosition Lib "bassmod.dll" (ByVal pos As Long) As Integer
Declare Function BASSMOD_MusicGetPosition Lib "bassmod.dll" () As Long
Declare Function BASSMOD_MusicSetSync Lib "bassmod.dll" (ByVal ptype As Long, ByVal param As Long, ByRef proc, ByVal user As Long) As Long
Declare Function BASSMOD_MusicRemoveSync Lib "bassmod.dll" (ByVal sync As Long) As Integer

Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (ByRef Destination As Any, ByRef Source As Any, ByVal length As Long)
Private Declare Function lstrlen Lib "kernel32" Alias "lstrlenA" (ByVal lpString As Long) As Long

Sub SYNCPROC(ByVal handle As Long, ByVal data As Long, ByVal user As Long)
    
    'CALLBACK FUNCTION !!!
    
    'Write what to do when sync function
    'is called, i.e screen flash etc.
    
End Sub

Public Function HiWord(lparam As Long) As Long
' This is the HIWORD of the lParam:
HiWord = lparam \ &H10000 And &HFFFF&
End Function
Public Function LoWord(lparam As Long) As Long
' This is the LOWORD of the lParam:
LoWord = lparam And &HFFFF&
End Function
Function MakeLong(LoWord As Long, HiWord As Long) As Long
'Replacement for the c++ Function MAKELONG
MakeLong = (LoWord And &HFFFF&) Or (HiWord * &H10000)
End Function

Public Function VBStrFromAnsiPtr(ByVal lpStr As Long) As String
Dim bStr() As Byte
Dim cChars As Long
On Error Resume Next
' Get the number of characters in the buffer
cChars = lstrlen(lpStr)

' Resize the byte array
ReDim bStr(0 To cChars - 1) As Byte

' Grab the ANSI buffer
Call CopyMemory(bStr(0), ByVal lpStr, cChars)

' Now convert to a VB Unicode string
VBStrFromAnsiPtr = StrConv(bStr, vbUnicode)
End Function
