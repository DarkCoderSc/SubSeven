/* BASSMOD simple test, copyright (c) 1999-2004 Ian Luck.
=========================================================
Imports: bassmod.lib, kernel32.lib, user32.lib, comdlg32.lib
*/

#include <windows.h>
#include <stdio.h>
#include "bassmod.h"

static HWND win=NULL;
static HINSTANCE inst;

static int length;		// MOD length
static int moving=0;	// user scrolling?

static OPENFILENAME ofn;
static char path[MAX_PATH];

/* display error messages */
void Error(char *es)
{
	char mes[200];
	sprintf(mes,"%s\n(error code: %d)",es,BASSMOD_ErrorGetCode());
	MessageBox(win,mes,"Error",0);
}

#define MESS(id,m,w,l) SendDlgItemMessage(win,id,m,(WPARAM)w,(LPARAM)l)

DWORD GetFlags()
{
	HWND h=win;
	DWORD flags=BASS_MUSIC_POSRESET; // stop notes when seeking
	DWORD i=MESS(21,CB_GETCURSEL,0,0);
	if (!i) flags|=BASS_MUSIC_NONINTER; // no interpolation
	i=MESS(22,CB_GETCURSEL,0,0);
	if (i==1) flags|=BASS_MUSIC_RAMP; // ramping
	if (i==2) flags|=BASS_MUSIC_RAMPS; // "sensitive"
	i=MESS(23,CB_GETCURSEL,0,0);
	if (i==1) flags|=BASS_MUSIC_SURROUND; // surround
	if (i==2) flags|=BASS_MUSIC_SURROUND2; // "mode2"
	return flags;
}

BOOL CALLBACK dialogproc(HWND h,UINT m,WPARAM w,LPARAM l)
{
	switch (m) {
		case WM_TIMER:
			if (!moving) {
				/* update "display" */
				char text[12];
				int pos=BASSMOD_MusicGetPosition();
				if (pos==-1) pos=0;
				else MESS(20,SBM_SETPOS,LOWORD(pos),1);
				sprintf(text,"%03d.%03d",LOWORD(pos),HIWORD(pos));
				MESS(15,WM_SETTEXT,0,text);
				sprintf(text,"%.1f%%",BASSMOD_GetCPU());
				MESS(16,WM_SETTEXT,0,text);
			}
			return 0;

		case WM_COMMAND:
			switch (LOWORD(w)) {
				case IDCANCEL:
					KillTimer(win,1);
					DestroyWindow(h);
					return 1;
				case 10:
					{
						char file[MAX_PATH]="";
						ofn.lpstrFilter="it/xm/s3m/mtm/mod/umx files\0*.xm;*.mod;*.s3m;*.it;*.mtm;*.umx\0All files\0*.*\0\0";
						ofn.lpstrFile=file;
						if (GetOpenFileName(&ofn)) {
							memcpy(path,file,ofn.nFileOffset);
							path[ofn.nFileOffset-1]=0;
							BASSMOD_MusicFree(); // free the current mod
							/* load the MOD (with looping, sensative ramping, surround sound, and note stopping with pos change) */
							if (BASSMOD_MusicLoad(FALSE,file,0,0,GetFlags())) {
								/* get the MOD length */
								length=BASSMOD_MusicGetLength(0);
								MESS(10,WM_SETTEXT,0,file);
								MESS(11,WM_SETTEXT,0,BASSMOD_MusicGetName());
								MESS(20,SBM_SETRANGE,0,length-1);
								BASSMOD_MusicPlay();
							} else { /* not a MOD */
								Error("Can't play the file");
								MESS(10,WM_SETTEXT,0,"click here to open a file...");
								MESS(11,WM_SETTEXT,0,"");
								MESS(20,SBM_SETRANGE,0,0);
							}
							MESS(20,SBM_SETPOS,0,1);
						}
					}
					return 1;
				case 12:
					BASSMOD_MusicPlay();
					return 1;
				case 13:
					BASSMOD_MusicPause();
					return 1;
				case 21:
				case 22:
				case 23:
					BASSMOD_MusicPlayEx(-1,GetFlags(),FALSE); // update flags
					break;
			}
			break;

		case WM_HSCROLL:
			{
				int a=SendMessage(l,SBM_GETPOS,0,0);
				switch (LOWORD(w)) {
					case SB_THUMBPOSITION:
						a=HIWORD(w);
						break;
					case SB_LINELEFT:
						a=max(a-1,0);
						break;
					case SB_LINERIGHT:
						if (a==(length-1)) return 1;
						a++;
						break;
					case SB_PAGELEFT:
						a=max(a-5,0);
						break;
					case SB_PAGERIGHT:
						if (a==(length-1)) return 1;
						a=min(a+5,length-1);
						break;
					case SB_THUMBTRACK:
						moving=1;	// the user's moving the scroller
					default:
						return 1;
				}
				/* change the position... */
				BASSMOD_MusicSetPosition(a);
				moving=0;
			}
			return 1;

		case WM_INITDIALOG:
			win=h;
			GetCurrentDirectory(MAX_PATH,path);
			memset(&ofn,0,sizeof(ofn));
			ofn.lStructSize=sizeof(ofn);
			ofn.hwndOwner=h;
			ofn.hInstance=inst;
			ofn.nMaxFile=MAX_PATH;
			ofn.lpstrInitialDir=path;
			ofn.Flags=OFN_HIDEREADONLY|OFN_EXPLORER;
			MESS(21,CB_ADDSTRING,0,"off");
			MESS(21,CB_ADDSTRING,0,"linear");
			MESS(21,CB_SETCURSEL,1,0);
			MESS(22,CB_ADDSTRING,0,"off");
			MESS(22,CB_ADDSTRING,0,"normal");
			MESS(22,CB_ADDSTRING,0,"sensitive");
			MESS(22,CB_SETCURSEL,2,0);
			MESS(23,CB_ADDSTRING,0,"off");
			MESS(23,CB_ADDSTRING,0,"mode1");
			MESS(23,CB_ADDSTRING,0,"mode2");
			MESS(23,CB_SETCURSEL,0,0);
			SetTimer(win,1,100,NULL);
			return 1;
	}
	return 0;
}

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,LPSTR lpCmdLine, int nCmdShow)
{
	inst=hInstance;

	/* Check that BASSMOD 2.0 was loaded */
	if (BASSMOD_GetVersion()!=MAKELONG(2,0)) {
		Error("BASSMOD version 2.0 was not loaded");
		return 0;
	}

	/* setup output - default device, 44100hz, stereo, 16 bits */
	if (!BASSMOD_Init(-1,44100,BASS_DEVICE_NOSYNC)) {
		Error("Can't initialize device");
		return 0;
	}
	
	DialogBox(inst,1000,0,&dialogproc);

	BASSMOD_Free();

	return 0;
}
