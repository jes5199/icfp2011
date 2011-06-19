@echo off

simulator\dist\build\getTestCases\getTestCases.exe test_cases
if errorlevel 1 goto done

REM simulator\dist\build\simdriver\simdriver.exe < test_cases\%1
bin\ltg.win32.exe alt < test_cases\%1

:done