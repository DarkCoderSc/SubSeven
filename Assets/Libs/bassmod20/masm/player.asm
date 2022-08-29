; #########################################################################
; Simple BASS Player in MASM32 by TBD
; (modified for BASSMOD)
;
; MASM32 package (c) Steve 'Hutch' Hutchesson http://www.movsd.com
; BASS engine (c) Ian Luck  http://www.un4seen.com/
; #########################################################################

      .486                      ; create 32 bit code
      .model flat, stdcall      ; 32 bit memory model
      option casemap :none      ; case sensitive

;     include files
;     ~~~~~~~~~~~~~
      include \masm32\include\windows.inc
      include \masm32\include\masm32.inc
      include \masm32\include\gdi32.inc
      include \masm32\include\user32.inc
      include \masm32\include\kernel32.inc
      include \masm32\include\Comctl32.inc
      include \masm32\include\comdlg32.inc
      include \masm32\include\winmm.inc

      include bassmod.inc


;     libraries
;     ~~~~~~~~~
      includelib \masm32\lib\masm32.lib
      includelib \masm32\lib\gdi32.lib
      includelib \masm32\lib\user32.lib
      includelib \masm32\lib\kernel32.lib
      includelib \masm32\lib\Comctl32.lib
      includelib \masm32\lib\comdlg32.lib
      includelib \masm32\lib\winmm.lib      
      includelib bassmod.lib
      

      WinMain          PROTO :DWORD,:DWORD,:DWORD,:DWORD
      WndProc          PROTO :DWORD,:DWORD,:DWORD,:DWORD
      TopXY            PROTO :DWORD,:DWORD
      GetFileName      PROTO :DWORD, :DWORD, :DWORD
      Do_Status        PROTO :DWORD
      RegisterWinClass PROTO :DWORD,:DWORD,:DWORD,:DWORD,:DWORD
      MsgLoop          PROTO
      Main             PROTO
      Level            PROTO :DWORD,:DWORD,:DWORD,:DWORD,:DWORD
      Do_ToolBar       PROTO :DWORD
      SetBmpColor      PROTO :DWORD
  
      wsprintfA PROTO C :DWORD,:VARARG
      wsprintf equ <wsprintfA>

      ;=============
      ; Local macros
      ;=============

      szText MACRO Name, Text:VARARG
        LOCAL lbl
          jmp lbl
            Name db Text,0
          lbl:
        ENDM

      m2m MACRO M1, M2
        push M2
        pop  M1
      ENDM

      return MACRO arg
        mov eax, arg
        ret
      ENDM

      DisplayMenu MACRO handl, IDnum
        invoke LoadMenu,hInstance,IDnum
        invoke SetMenu,handl,eax
      ENDM

      DisplayWindow MACRO handl, ShowStyle
        invoke ShowWindow,handl, ShowStyle
        invoke UpdateWindow,handl
      ENDM

      ; ---------------------------
      ; macros for creating toolbar
      ; ---------------------------


      TBextraData MACRO
        mov tbb.fsState,   TBSTATE_ENABLED
        mov tbb.dwData,    0
        mov tbb.iString,   0
      ENDM

      ; ------------------------------

      TBbutton MACRO bID, cID, bStyle
        mov tbb.iBitmap,   bID  ;; button  ID number
        mov tbb.idCommand, cID  ;; command ID number
        mov tbb.fsStyle,   bStyle 
        invoke SendMessage,hToolBar,TB_ADDBUTTONS,1,ADDR tbb
      ENDM

      ; ------------------------------

      TBblank MACRO
        mov tbb.iBitmap,   0
        mov tbb.idCommand, 0
        mov tbb.fsStyle,   TBSTYLE_SEP
        invoke SendMessage,hToolBar,TB_ADDBUTTONS,1,ADDR tbb
      ENDM

      ; ------------------------------

      Create_Tool_Bar MACRO Wd, Ht

        szText tbClass,"ToolbarWindow32"

        invoke CreateWindowEx,0,
                              ADDR tbClass,
                              ADDR tbClass,
                              WS_CHILD or WS_VISIBLE or TBSTYLE_FLAT,
                              0,0,500,20,
                              hWin,NULL,
                              hInstance,NULL

        mov hToolBar, eax
    
        invoke SendMessage,hToolBar,TB_BUTTONSTRUCTSIZE,sizeof TBBUTTON,0
    
        ; ---------------------------------------
        ; Put width & height of bitmap into DWORD
        ; ---------------------------------------
        mov  ecx,Wd  ;; loword = bitmap Width
        mov  eax,Ht  ;; hiword = bitmap Height
        shl  eax,16
        mov  ax, cx

        mov bSize, eax
    
        invoke SendMessage,hToolBar,TB_SETBITMAPSIZE,0,bSize
    
        invoke SetBmpColor,hTbBmp
        mov hTbBmp,eax
    
        mov tbab.hInst, 0
        m2m tbab.nID,   hTbBmp
        invoke SendMessage,hToolBar,TB_ADDBITMAP,12,ADDR tbab
    
        invoke SendMessage,hToolBar,TB_SETBUTTONSIZE,0,bSize
      ENDM


    ; --------------------------------------------
    ; equates to use directly in the message loop
    ; --------------------------------------------
        m_hWnd   equ <msg.hwnd>
        m_Msg    equ <msg.message>
        m_wParam equ <msg.wParam>
        m_lParam equ <msg.lParam>

.data?
      hInstance     dd ?
      CommandLine   dd ?
      hCursor       dd ?
      hWnd          dd ?
      ModName       dd ?
      MusicFile     dd ?         
      hStatus       dd ?
      hTbBmp        dd ?
      hToolBar      dd ?
      hWinLevel     dd ?
      VIS_WIDTH     = 52
      VIS_HEIGHT    = 4
      ofn           OPENFILENAME <>  ; structure
      bmp           BITMAPINFO <>
      

      
.code

; #########################################################################

start:

      invoke InitCommonControls

      invoke GetModuleHandle, NULL
      mov hInstance, eax

      invoke LoadCursor,NULL,IDC_ARROW
      mov hCursor, eax

      call Main

      invoke ExitProcess,eax

; data 
      szClassName    db "BASSMOD_Player",0
      szDisplayName  db "Simple BASSMOD Player",0
      szAboutText    db "BASSPlayer",13,10,"Copyright © TBD 2oo2",0
      szOpen         db "Open A File",0
      szFiles        db "MOD, S3M, IT, XM, MTM, UMX Files",0,"*.mod;*.s3m;*.it;*.xm;*.mtm;*.umx",0,0
      szFormatString db "%03u:%03u | %s",0

; #########################################################################

Main proc

    LOCAL Wwd:DWORD,Wht:DWORD,Wtx:DWORD,Wty:DWORD

    invoke RegisterWinClass,ADDR WndProc,ADDR szClassName,
                       0,hCursor,COLOR_BTNFACE+1

    invoke CreateWindowEx,WS_EX_LEFT,
                          ADDR szClassName,
                          ADDR szDisplayName,
                          WS_VISIBLE OR WS_CAPTION OR WS_SYSMENU OR WS_MINIMIZEBOX,
                          300,300,225,93,
                          NULL,NULL,
                          hInstance,NULL
    mov hWnd,eax

    DisplayMenu hWnd,600
    DisplayWindow hWnd,SW_SHOWNORMAL

    call MsgLoop
    ret

Main endp

; #########################################################################

RegisterWinClass proc lpWndProc:DWORD, lpClassName:DWORD,
                      Icon:DWORD, Cursor:DWORD, bColor:DWORD

    LOCAL wc:WNDCLASSEX

    mov wc.cbSize,         sizeof WNDCLASSEX
    mov wc.style,          CS_BYTEALIGNCLIENT or \
                           CS_BYTEALIGNWINDOW
    m2m wc.lpfnWndProc,    lpWndProc
    mov wc.cbClsExtra,     NULL
    mov wc.cbWndExtra,     NULL
    m2m wc.hInstance,      hInstance
    m2m wc.hbrBackground,  bColor
    mov wc.lpszMenuName,   NULL
    m2m wc.lpszClassName,  lpClassName
    m2m wc.hIcon,          Icon
    m2m wc.hCursor,        Cursor
    m2m wc.hIconSm,        Icon

    invoke RegisterClassEx, ADDR wc

    ret

RegisterWinClass endp

; ########################################################################

MsgLoop proc

    LOCAL msg:MSG

    StartLoop:
      invoke GetMessage,ADDR msg,NULL,0,0
      cmp eax, 0
      je ExitLoop
      invoke TranslateMessage, ADDR msg
      invoke DispatchMessage,  ADDR msg
      jmp StartLoop
    ExitLoop:

    mov eax, msg.wParam
    ret

MsgLoop endp

; #########################################################################

WndProc proc hWin   :DWORD,
             uMsg   :DWORD,
             wParam :DWORD,
             lParam :DWORD

    LOCAL var    :DWORD
    LOCAL caW    :DWORD
    LOCAL caH    :DWORD
    LOCAL Rct    :RECT
    LOCAL buffer1[128]:BYTE  ; these are two spare buffers
    LOCAL buffer2[128]:BYTE  ; for text manipulation etc..
    LOCAL szFileName[260]:BYTE

    .if uMsg == WM_COMMAND
    ;======== toolbar commands ========
        .if wParam == 50
            invoke BASSMOD_MusicPlay    ; play tune
            test eax,eax
            jz error
            invoke SetTimer, hWin, NULL, 50, NULL ; 

        .elseif wParam == 51
            invoke BASSMOD_MusicPause

        .elseif wParam == 52
            invoke BASSMOD_MusicGetPosition
            dec ax
            and eax, 0000000011111111b
            invoke BASSMOD_MusicSetPosition, eax

        .elseif wParam == 53
            invoke BASSMOD_MusicGetPosition
            inc ax
            and eax, 0000000011111111b
            invoke BASSMOD_MusicSetPosition, eax
        .endif

    ;======== menu commands ========

        .if wParam == 1001
            mov szFileName[0],0     ; set 1st byte to zero

            mov ofn.lStructSize, sizeof OPENFILENAME
            m2m ofn.hWndOwner,   hWnd
            m2m ofn.hInstance,   hInstance
            m2m ofn.lpstrFilter, offset szFiles             
            mov eax, ebp
            sub eax, 220h
            push eax
            pop ofn.lpstrFile
            mov ofn.nMaxFile,    sizeof szFileName
            m2m ofn.lpstrTitle,  offset szOpen
            mov ofn.Flags,       OFN_EXPLORER or OFN_FILEMUSTEXIST or \
                                 OFN_LONGNAMES
        
            invoke GetOpenFileName,ADDR ofn
            cmp szFileName[0],0     ; zero if cancel pressed in dlgbox
            je @F

            invoke BASSMOD_MusicFree
            invoke BASSMOD_MusicLoad, 0, ADDR szFileName, 0, 0, BASS_MUSIC_LOOP OR \ 
                   BASS_MUSIC_POSRESET OR BASS_MUSIC_RAMPS            ; load musicfile
            test eax,eax
            jz error
        
            invoke BASSMOD_MusicGetName
            mov [ModName], eax

            invoke SendMessage, hWin, WM_COMMAND, 50, 0 

            @@:

        .endif

        .if wParam == 1010
            invoke SendMessage,hWin,WM_SYSCOMMAND,SC_CLOSE,NULL

        .elseif wParam == 1900
            invoke MessageBox, 0, ADDR szAboutText, ADDR szDisplayName, 0            
        .endif
    ;====== end menu commands ======
    .elseif uMsg == WM_TIMER
        invoke BASSMOD_MusicGetPosition
        mov edx, eax
        and eax, 0000000011111111b                      
        sar edx, 16
        invoke wsprintf, ADDR buffer1, ADDR szFormatString, eax, edx, ModName
        invoke SendMessage, hStatus, SB_SETTEXT, 0, ADDR buffer1    
                   
    .elseif uMsg == WM_CREATE
        invoke Do_ToolBar,hWin
        invoke Do_Status,hWin

        invoke BASSMOD_Init, -1, 44100, 0
        test eax,eax                                            
        jz error                                                
        							
    .elseif uMsg == WM_SYSCOLORCHANGE
        invoke Do_ToolBar,hWin
     
    .elseif uMsg == WM_KEYDOWN
        .IF wParam == VK_LEFT
            invoke SendMessage, hWin, WM_COMMAND, 52, 0 
        .ELSEIF wParam == VK_RIGHT        
            invoke SendMessage, hWin, WM_COMMAND, 53, 0 
        .ENDIF    

    .elseif uMsg == WM_SIZE                                    ; needed to update the status
        invoke SendMessage,hToolBar,TB_AUTOSIZE,0,0
        invoke MoveWindow,hStatus,0,0,0,0,TRUE

    .elseif uMsg == WM_CLOSE
    .elseif uMsg == WM_DESTROY
        invoke KillTimer, hWin, NULL
        invoke BASSMOD_Free
        .if hTbBmp != NULL  
          invoke DeleteObject, hTbBmp  
        .endif
        invoke PostQuitMessage,NULL        
        return 0

    .endif

    invoke DefWindowProc,hWin,uMsg,wParam,lParam
error:
    ret

WndProc endp

; ##########################################################################

Do_ToolBar proc hWin :DWORD

    LOCAL bSize :DWORD
    LOCAL tbab  :TBADDBITMAP
    LOCAL tbb   :TBBUTTON

    invoke LoadBitmap,hInstance,750
    mov hTbBmp,eax

    Create_Tool_Bar 16, 16

    TBextraData     ; additional data for TBBUTTON structure

    TBblank
    TBbutton  0,  50, TBSTYLE_BUTTON
    TBbutton  1,  51, TBSTYLE_BUTTON
    TBbutton  2,  52, TBSTYLE_BUTTON
    TBbutton  3,  53, TBSTYLE_BUTTON

    ret

Do_ToolBar endp

; #########################################################################

Do_Status proc hParent:DWORD

    LOCAL sbParts[ 4] :DWORD

    invoke CreateStatusWindow,WS_CHILD or WS_VISIBLE,NULL, hParent, 200
    mov hStatus, eax

    ret

Do_Status endp

; ########################################################################

SetBmpColor proc hBitmap:DWORD

    LOCAL mDC       :DWORD
    LOCAL hBrush    :DWORD
    LOCAL hOldBmp   :DWORD
    LOCAL hReturn   :DWORD
    LOCAL hOldBrush :DWORD

    invoke CreateCompatibleDC,NULL
    mov mDC,eax

    invoke SelectObject,mDC,hBitmap
    mov hOldBmp,eax

    invoke GetSysColor,COLOR_BTNFACE
    invoke CreateSolidBrush,eax
    mov hBrush,eax

    invoke SelectObject,mDC,hBrush
    mov hOldBrush,eax

    invoke GetPixel,mDC,1,1   
    invoke ExtFloodFill,mDC,1,1,eax,FLOODFILLSURFACE

    invoke SelectObject,mDC,hOldBrush
    invoke DeleteObject,hBrush

    invoke SelectObject,mDC,hBitmap
    mov hReturn,eax
    invoke DeleteDC,mDC

    mov eax,hReturn

    ret

SetBmpColor endp

; ##################################################################################

end start

; ############################################################### [ END ] ##########

