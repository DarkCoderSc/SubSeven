VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form frmMODtest 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "BASSMOD - simple test"
   ClientHeight    =   2175
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4710
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2175
   ScaleWidth      =   4710
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tmrMODTest 
      Interval        =   100
      Left            =   0
      Top             =   1680
   End
   Begin VB.CommandButton btnPlayPause 
      Caption         =   "PAUSE"
      Height          =   255
      Index           =   1
      Left            =   3720
      TabIndex        =   6
      Top             =   840
      Width           =   735
   End
   Begin VB.CommandButton btnPlayPause 
      Caption         =   "PLAY"
      Height          =   255
      Index           =   0
      Left            =   2880
      TabIndex        =   5
      Top             =   840
      Width           =   735
   End
   Begin VB.HScrollBar hsPosition 
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   1200
      Width           =   4455
   End
   Begin VB.ComboBox cmbIRS 
      Height          =   315
      Index           =   2
      Left            =   3120
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Top             =   1800
      Width           =   1095
   End
   Begin VB.ComboBox cmbIRS 
      Height          =   315
      Index           =   1
      Left            =   1800
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Top             =   1800
      Width           =   1095
   End
   Begin VB.ComboBox cmbIRS 
      Height          =   315
      Index           =   0
      Left            =   480
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   1800
      Width           =   1095
   End
   Begin MSComDlg.CommonDialog cmdMODtest 
      Left            =   4080
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton btnOpen 
      Caption         =   "click here to open a file..."
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   4455
   End
   Begin VB.Label lblCPU 
      Alignment       =   2  'Center
      BorderStyle     =   1  'Fixed Single
      Caption         =   "0.0%"
      Height          =   255
      Left            =   1920
      TabIndex        =   12
      Top             =   840
      Width           =   540
   End
   Begin VB.Label lblPos 
      Alignment       =   2  'Center
      BorderStyle     =   1  'Fixed Single
      Caption         =   "000.000"
      Height          =   255
      Left            =   360
      TabIndex        =   11
      Top             =   860
      Width           =   1140
   End
   Begin VB.Label lblSong 
      Alignment       =   2  'Center
      Height          =   255
      Left            =   120
      TabIndex        =   10
      Top             =   560
      Width           =   4455
   End
   Begin VB.Label lblIRS 
      AutoSize        =   -1  'True
      Caption         =   "Surround"
      Height          =   195
      Index           =   2
      Left            =   3360
      TabIndex        =   9
      Top             =   1560
      Width           =   645
   End
   Begin VB.Label lblIRS 
      AutoSize        =   -1  'True
      Caption         =   "Ramping"
      Height          =   195
      Index           =   1
      Left            =   2040
      TabIndex        =   8
      Top             =   1560
      Width           =   630
   End
   Begin VB.Label lblIRS 
      AutoSize        =   -1  'True
      Caption         =   "Interpolation"
      Height          =   195
      Index           =   0
      Left            =   600
      TabIndex        =   7
      Top             =   1560
      Width           =   870
   End
End
Attribute VB_Name = "frmMODtest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'///////////////////////////////////////////////////////////////////////////
' frmMODtest.frm - Copyright (c) 2007 (: JOBnik! :) [Arthur Aminov, ISRAEL]
'                                                   [http://www.jobnik.org]
'                                                   [  jobnik@jobnik.org  ]
' BASSMOD - simple test
' Originally translated from - modtest.c - Example of Ian Luck
'///////////////////////////////////////////////////////////////////////////

Option Explicit

Dim length As Long      ' MOD length
Dim moving As Long      ' user scrolling?
Dim tmpTime As Boolean

' display error messages
Sub Error_(ByVal es As String)
    Call MsgBox(es & vbCrLf & vbCrLf & "error code: " & BASSMOD_ErrorGetCode, vbExclamation, "Error")
End Sub

Function GetFlags() As Long
    Dim flags As Long
    flags = BASS_MUSIC_POSRESET ' stop notes when seeking
    Dim i As Long
    i = cmbIRS(0).ListIndex
    If (i = 0) Then flags = flags Or BASS_MUSIC_NONINTER ' no interpolation
    i = cmbIRS(1).ListIndex
    If (i = 1) Then flags = flags Or BASS_MUSIC_RAMP ' ramping
    If (i = 2) Then flags = flags Or BASS_MUSIC_RAMPS ' "sensitive"
    i = cmbIRS(2).ListIndex
    If (i = 1) Then flags = flags Or BASS_MUSIC_SURROUND ' surround
    If (i = 2) Then flags = flags Or BASS_MUSIC_SURROUND2 ' "mode2"
    GetFlags = flags
End Function

Private Sub Form_Load()
    ' change and set the current path, to prevent from VB not finding BASSMOD.DLL
    ChDrive App.Path
    ChDir App.Path

    ' check that BASSMOD 2.0 was loaded
    If (BASSMOD_GetVersion <> MakeLong(2, 0)) Then
        Call MsgBox("BASSMOD version 2.0 was not loaded", vbCritical)
        End
    End If

    ' setup output - default device, 44100hz, stereo, 16 bits
    If (BASSMOD_Init(-1, 44100, BASS_DEVICE_NOSYNC) = 0) Then
        Call Error_("Can't initialize device")
        Unload Me
    End If

    moving = 0

    ' set Interpolation ComboBox
    With cmbIRS(0)
        .AddItem "off"
        .AddItem "linear"
        .ListIndex = 1  ' default "linear"
    End With

    ' set Ramping ComboBox
    With cmbIRS(1)
        .AddItem "off"
        .AddItem "normal"
        .AddItem "sensitive"
        .ListIndex = 2  ' default "sensitive"
    End With

    ' set Surround ComboBox
    With cmbIRS(2)
        .AddItem "off"
        .AddItem "mode1"
        .AddItem "mode2"
        .ListIndex = 0  ' default "off"
    End With
End Sub

Private Sub Form_Unload(Cancel As Integer)
     Call BASSMOD_Free
End Sub

Private Sub btnPlayPause_Click(Index As Integer)
    Select Case Index
        Case 0: Call BASSMOD_MusicPlay
        Case 1: Call BASSMOD_MusicPause
    End Select
End Sub

Private Sub cmbIRS_Click(Index As Integer)
    Call BASSMOD_MusicPlayEx(-1, GetFlags(), BASSFALSE)  ' update flags
End Sub

Private Sub btnOpen_Click()
    On Local Error Resume Next    ' if Cancel pressed...

    With cmdMODtest
        .CancelError = True
        .flags = cdlOFNExplorer Or cdlOFNFileMustExist Or cdlOFNHideReadOnly
        .DialogTitle = "Open"
        .Filter = "it/xm/s3m/mtm/mod/umx files|*.xm;*.mod;*.s3m;*.it;*.mtm;*.umx|All files|*.*"
        .ShowOpen
    End With

    ' if cancel was pressed, exit the procedure
    If Err.Number = 32755 Then Exit Sub

    ' free the current mod
    Call BASSMOD_MusicFree

    ' load the MOD (with looping, sensative ramping, surround sound, and note stopping with pos change)
    If (BASSMOD_MusicLoad(BASSFALSE, cmdMODtest.FileName, 0, 0, GetFlags())) Then
        ' get the MOD length
        length = BASSMOD_MusicGetLength(0)
        btnOpen.Caption = GetFileName(cmdMODtest.FileName)
        lblSong.Caption = BASSMOD_MusicGetName
        hsPosition.Max = length - 1
        Call BASSMOD_MusicPlay
    Else    ' not a MOD
        Call Error_("Can't play the file")
        btnOpen.Caption = "click here to open a file..."
        hsPosition.Max = 0
        Exit Sub
    End If
    hsPosition.Value = 0
End Sub

' change the position...
Private Sub hsPosition_Change()
    If (Not tmpTime) Then Call BASSMOD_MusicSetPosition(hsPosition.Value)
    tmrMODTest.Enabled = True
    moving = 0
End Sub

Private Sub hsPosition_Scroll()
    tmrMODTest.Enabled = False ' the user's moving the scroller
    moving = 1
End Sub

Private Sub tmrMODTest_Timer()
    If (moving = 0) Then
        ' update "display"
        Dim pos As Long
        pos = BASSMOD_MusicGetPosition()
        If (pos = -1) Then
            pos = 0
        Else
            tmpTime = True
            hsPosition.Value = LoWord(pos)
            tmpTime = False
        End If
        lblPos.Caption = Format(LoWord(pos), "000") & "." & Format(HiWord(pos), "000")
        lblCPU.Caption = Format(BASSMOD_GetCPU(), "0.0") & "%"
    End If
    
End Sub

'--------------------
' useful function :)
'--------------------

' get file name from file path
Public Function GetFileName(ByVal fp As String) As String
    GetFileName = Mid(fp, InStrRev(fp, "\") + 1)
End Function
