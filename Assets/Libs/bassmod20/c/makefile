CC = gcc
RES = windres
FLAGS = -O2
GUIFLAGS = -Wl,--subsystem,windows

all: contest.exe modtest.exe

contest.exe: contest.c
	$(CC) $(FLAGS) contest.c bassmod.lib -lwinmm -o $@

modtest.exe: modtest.c modtest.rc
	$(RES) -i modtest.rc -o rsrc.obj
	$(CC) $(FLAGS) modtest.c rsrc.obj bassmod.lib -lcomdlg32 $(GUIFLAGS) -o $@

clean:
	del contest.exe
	del modtest.exe
	del rsrc.obj
