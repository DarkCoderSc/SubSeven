:: Clean Compiled Units
del /s /q *.dcu
del /s /q *.tmp
del /s /q *.rsm
del /s /q *.dproj.local
del /s /q *.identcache

:: Clean Embarcadero Directories
for /f "usebackq" %%a in (`"dir /ad/b/s __history"`) do rmdir /q /s "%%a"
for /f "usebackq" %%a in (`"dir /ad/b/s __recovery"`) do rmdir /q /s "%%a"

:: Clean Compiled Directories
rmdir /q /s "Viewer\Win64\"
rmdir /q /s "Viewer\Win32\"
rmdir /q /s "Viewer\VCL\Win32\"

rmdir /q /s "Service\Win64\"
rmdir /q /s "Service\Win32\"

rmdir /q /s "Helper\Win64\"
rmdir /q /s "Helper\Win32\"

rmdir /q /s "CertGen\Win64\"
rmdir /q /s "CertGen\Win32\"

rmdir /q /s "Tray\Win64\"
rmdir /q /s "Tray\Win32\"
rmdir /q /s "Tray\VCL\Win32\"

pause