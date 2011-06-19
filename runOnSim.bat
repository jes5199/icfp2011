@echo off

simulator\dist\build\getTestCases\getTestCases.exe test_cases
if errorlevel 1 goto done

simulator\dist\build\simdriver\simdriver.exe < test_cases\%1
REM bin\ltg.win32.exe alt < test_cases\%1

:done