#include <stdio.h>
#include "bassmod.h"

#ifdef WIN32
#include <conio.h>
#include <mmsystem.h>
#else
#include <sys/types.h>
#include <sys/time.h>
#include <termios.h>

#define Sleep(x) usleep(x*1000)

DWORD timeGetTime()
{
	struct timeval tv;
	gettimeofday(&tv,0);
	return tv.tv_sec*1000+tv.tv_usec/1000;
}

int _kbhit()
{
	int r;
	fd_set rfds;
	struct timeval tv;
	struct termios term,oterm;
	tcgetattr(0,&oterm);
	memcpy(&term,&oterm,sizeof(term));
	cfmakeraw(&term);
	tcsetattr(0,TCSANOW,&term);
	FD_ZERO(&rfds);
	FD_SET(0,&rfds);
	tv.tv_sec=tv.tv_usec=0;
	r=select(1,&rfds,NULL,NULL,&tv);
	tcsetattr(0,TCSANOW,&oterm);
	return r;
}
#endif

/* display error messages */
void Error(char *text) 
{
	printf("Error(%d): %s\n",BASSMOD_ErrorGetCode(),text);
	BASSMOD_Free();
	exit(0);
}

static int starttime;

void CALLBACK LoopSync(HSYNC handle, DWORD data, DWORD user)
{
	starttime=timeGetTime();
}

void main(int argc, char **argv)
{
	int t,p;

	printf("Simple console BASSMOD example : IT/XM/S3M/MTM/MOD/UMX player\n"
			"-------------------------------------------------------------\n");

	/* check that BASSMOD 2.0 was loaded */
	if (BASSMOD_GetVersion()!=MAKELONG(2,0)) {
		printf("BASSMOD version 2.0 was not loaded\n");
		return;
	}

	if (argc!=2) {
		printf("\tusage: contest <file>\n");
		return;
	}

	if (!BASSMOD_Init(-1,44100,0))
		Error("Can't initialize device");

	/* try loading the MOD (with looping, sensitive ramping, surround sound and calculate the duration) */
	if (!BASSMOD_MusicLoad(FALSE,argv[1],0,0,BASS_MUSIC_LOOP|BASS_MUSIC_RAMPS|BASS_MUSIC_SURROUND|BASS_MUSIC_CALCLEN))
		Error("Can't play the file");

	/* set a synchronizer for when the MOD reaches the end */
	BASSMOD_MusicSetSync(BASS_SYNC_END,0,&LoopSync,0);

	printf("playing \"%s\" [%d orders]",BASSMOD_MusicGetName(),BASSMOD_MusicGetLength(FALSE));

	/* display the time length */
	if (t=BASSMOD_MusicGetLength(TRUE)) {
		t/=176400;
		printf(" %d:%02d\n",t/60,t%60);
	} else /* no time length available */
		printf("\n");

	BASSMOD_MusicPlay();
	starttime=timeGetTime();

	while (!_kbhit()) {
		p=BASSMOD_MusicGetPosition();
		t=(timeGetTime()-starttime)/1000;
		printf("pos: %03d.%03d - time: %d:%02d - cpu: %.2f%%  \r",LOWORD(p),HIWORD(p),t/60,t%60,BASSMOD_GetCPU());
		fflush(stdout);
		Sleep(50);
	}
	printf("                                                                         \n");

	BASSMOD_Free();
}
