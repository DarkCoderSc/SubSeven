@echo off
del *.obj
del *.res
rc /v rsrc.rc
cvtres /machine:ix86 rsrc.res
ml /c /coff player.asm
link /SUBSYSTEM:WINDOWS player.obj rsrc.obj
