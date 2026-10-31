@echo off
chcp 65001 >nul
setlocal
cd /d "%~dp0"
title kitJoin

echo.
echo  ========================================
echo   kitJoin - KIT 資料處理工具
echo  ========================================
echo.
echo  資料夾：%CD%
echo.

if not exist "%~dp0scripts\run_app.R" (
  echo [錯誤] 找不到 scripts\run_app.R，請重新解壓縮 ZIP 檔。
  pause & exit /b 1
)
if not exist "%~dp0app.R" (
  echo [錯誤] 找不到 app.R，請重新解壓縮 ZIP 檔。
  pause & exit /b 1
)

set "RSCRIPT="

where Rscript.exe >nul 2>&1
if not errorlevel 1 (
  for /f "delims=" %%i in ('where Rscript.exe') do ( set "RSCRIPT=%%i" & goto run )
)

for /f "tokens=2*" %%a in ('reg query "HKLM\SOFTWARE\R-core\R64" /v InstallPath 2^>nul') do (
  if exist "%%b\bin\x64\Rscript.exe" ( set "RSCRIPT=%%b\bin\x64\Rscript.exe" & goto run )
  if exist "%%b\bin\Rscript.exe"     ( set "RSCRIPT=%%b\bin\Rscript.exe"     & goto run )
)

for /f "tokens=2*" %%a in ('reg query "HKCU\SOFTWARE\R-core\R64" /v InstallPath 2^>nul') do (
  if exist "%%b\bin\x64\Rscript.exe" ( set "RSCRIPT=%%b\bin\x64\Rscript.exe" & goto run )
  if exist "%%b\bin\Rscript.exe"     ( set "RSCRIPT=%%b\bin\Rscript.exe"     & goto run )
)

for /f "tokens=2*" %%a in ('reg query "HKLM\SOFTWARE\R-core\R" /v InstallPath 2^>nul') do (
  if exist "%%b\bin\x64\Rscript.exe" ( set "RSCRIPT=%%b\bin\x64\Rscript.exe" & goto run )
  if exist "%%b\bin\Rscript.exe"     ( set "RSCRIPT=%%b\bin\Rscript.exe"     & goto run )
)

if exist "%ProgramFiles%\R" (
  for /f "delims=" %%i in ('dir /b /ad /o-d "%ProgramFiles%\R\R-*" 2^>nul') do (
    if exist "%ProgramFiles%\R\%%i\bin\x64\Rscript.exe" (
      set "RSCRIPT=%ProgramFiles%\R\%%i\bin\x64\Rscript.exe" & goto run
    )
    if exist "%ProgramFiles%\R\%%i\bin\Rscript.exe" (
      set "RSCRIPT=%ProgramFiles%\R\%%i\bin\Rscript.exe" & goto run
    )
  )
)

echo [錯誤] 找不到 R。請先安裝 R：https://cran.csie.ntu.edu.tw/
echo        安裝時一路按「下一步」即可，裝好後再執行一次 run.bat。
pause & exit /b 1

:run
echo 使用的 R：%RSCRIPT%
echo.
echo 檢查套件中（第一次使用需要安裝套件，約 3-10 分鐘，請保持網路連線）...
echo 使用期間請不要關閉這個視窗；用完直接關閉即可結束 kitJoin。
echo.

set "ROOT=%~dp0."
"%RSCRIPT%" "%~dp0scripts\run_app.R" "%ROOT%"
if errorlevel 1 (
  echo.
  echo [錯誤] kitJoin 啟動失敗。
  echo        錯誤原因記錄在：%~dp0kitjoin_log.txt
  echo        請把這個檔案的內容貼到 https://github.com/jiewenTsai/kitJoin/issues 回報。
  echo.
  pause
  exit /b 1
)

endlocal
exit /b 0
