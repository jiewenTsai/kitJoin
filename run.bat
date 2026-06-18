@echo off
setlocal
cd /d "%~dp0"
title kitJoin

echo.
echo  ========================================
echo   kitJoin - KIT Data Join Tool
echo  ========================================
echo.
echo  Dir: %CD%
echo.

if not exist "%~dp0scripts\run_app.R" (
  echo [ERROR] scripts\run_app.R not found. Please re-extract the ZIP.
  pause & exit /b 1
)
if not exist "%~dp0inst\shiny-app\app.R" (
  echo [ERROR] inst\shiny-app not found. Please re-extract the ZIP.
  pause & exit /b 1
)

set RSCRIPT=

where Rscript.exe >nul 2>&1
if not errorlevel 1 (
  for /f "delims=" %%i in ('where Rscript.exe') do ( set RSCRIPT=%%i & goto run )
)

for /f "tokens=2*" %%a in ('reg query "HKLM\SOFTWARE\R-core\R64" /v InstallPath 2^>nul') do (
  if exist "%%b\bin\x64\Rscript.exe" ( set RSCRIPT=%%b\bin\x64\Rscript.exe & goto run )
  if exist "%%b\bin\Rscript.exe"     ( set RSCRIPT=%%b\bin\Rscript.exe     & goto run )
)

for /f "tokens=2*" %%a in ('reg query "HKLM\SOFTWARE\R-core\R" /v InstallPath 2^>nul') do (
  if exist "%%b\bin\x64\Rscript.exe" ( set RSCRIPT=%%b\bin\x64\Rscript.exe & goto run )
  if exist "%%b\bin\Rscript.exe"     ( set RSCRIPT=%%b\bin\Rscript.exe     & goto run )
)

if exist "%ProgramFiles%\R" (
  for /f "delims=" %%i in ('dir /b /ad /o-n "%ProgramFiles%\R\R-*" 2^>nul') do (
    if exist "%ProgramFiles%\R\%%i\bin\x64\Rscript.exe" (
      set RSCRIPT=%ProgramFiles%\R\%%i\bin\x64\Rscript.exe & goto run
    )
    if exist "%ProgramFiles%\R\%%i\bin\Rscript.exe" (
      set RSCRIPT=%ProgramFiles%\R\%%i\bin\Rscript.exe & goto run
    )
  )
)

echo [ERROR] R not found. Install from: https://cran.r-project.org/
pause & exit /b 1

:run
echo Using: %RSCRIPT%
echo.
echo Checking packages (first run may take a few minutes)...
echo.

set ROOT=%~dp0.
"%RSCRIPT%" "%~dp0scripts\run_app.R" "%ROOT%"
if errorlevel 1 (
  echo.
  echo [ERROR] Launch failed.
  echo.
  pause
  exit /b 1
)

endlocal
exit /b 0
