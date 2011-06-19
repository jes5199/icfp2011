@echo off

bin\ltg.win32.exe match simulator\dist\build\braindriver\braindriver.exe simulator\dist\build\braindriver\braindriver.exe
if errorlevel 1 goto done

:done