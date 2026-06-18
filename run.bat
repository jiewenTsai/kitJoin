@echo off
setlocal EnableExtensions EnableDelayedExpansion
chcp 65001 >nul 2>&1
cd /d "%~dp0"

title KIT 資料串接工具

echo.
echo  ========================================
echo   KIT 資料串接工具  kitJoin
echo  ========================================
echo.
echo  工作目錄：%CD%
echo.

if not exist "%~dp0scripts\run_app.R" (
  echo [錯誤] 找不到 scripts\run_app.R
  echo 請確認 ZIP 已完整解壓，並在專案根目錄雙擊 run.bat
  echo.
  pause
  exit /b 1
)

set "RSCRIPT="

where Rscript.exe >nul 2>&1
if not errorlevel 1 (
  for /f "delims=" %%i in ('where Rscript.exe 2^>nul') do (
    set "RSCRIPT=%%i"
    goto :found_r
  )
)

where Rscript >nul 2>&1
if not errorlevel 1 (
  for /f "delims=" %%i in ('where Rscript 2^>nul') do (
    set "RSCRIPT=%%i"
    goto :found_r
  )
)

for /f "tokens=2*" %%a in ('reg query "HKLM\SOFTWARE\R-core\R64" /v InstallPath 2^>nul') do (
  if exist "%%b\bin\x64\Rscript.exe" set "RSCRIPT=%%b\bin\x64\Rscript.exe"
  if exist "%%b\bin\Rscript.exe"     set "RSCRIPT=%%b\bin\Rscript.exe"
  if defined RSCRIPT goto :found_r
)

for /f "tokens=2*" %%a in ('reg query "HKLM\SOFTWARE\R-core\R" /v InstallPath 2^>nul') do (
  if exist "%%b\bin\x64\Rscript.exe" set "RSCRIPT=%%b\bin\x64\Rscript.exe"
  if exist "%%b\bin\Rscript.exe"     set "RSCRIPT=%%b\bin\Rscript.exe"
  if defined RSCRIPT goto :found_r
)

if exist "%ProgramFiles%\R" (
  for /f "delims=" %%i in ('dir /b /ad /o-n "%ProgramFiles%\R\R-*" 2^>nul') do (
    if exist "%ProgramFiles%\R\%%i\bin\x64\Rscript.exe" (
      set "RSCRIPT=%ProgramFiles%\R\%%i\bin\x64\Rscript.exe"
      goto :found_r
    )
    if exist "%ProgramFiles%\R\%%i\bin\Rscript.exe" (
      set "RSCRIPT=%ProgramFiles%\R\%%i\bin\Rscript.exe"
      goto :found_r
    )
  )
)

echo [錯誤] 找不到 R。
echo.
echo 請先安裝 R：https://cran.r-project.org/
echo 安裝時建議勾選「Add R to PATH」。
echo.
pause
exit /b 1

:found_r
echo 使用 R：!RSCRIPT!
echo.
echo 首次執行會自動安裝缺少的套件，請稍候…
echo 套件就緒後瀏覽器將自動開啟，請耐心等待（約 10–60 秒）。
echo.

if not defined KITJOIN_PORT set "KITJOIN_PORT=7600"

rem 背景輪詢：curl 確認伺服器就緒後由 Windows 開瀏覽器（不依賴 R 的 browseURL）
start "" cmd /c "for /l %%i in (1,1,300) do (curl -fs -o nul http://127.0.0.1:%KITJOIN_PORT% 2>nul && (start http://127.0.0.1:%KITJOIN_PORT% & exit /b 0)) & timeout /t 1 /nobreak >nul"

rem %~dp0 結尾有反斜線會跳脫引號，改用 %CD% 傳根目錄
set "ROOT_DIR=%CD%"
"!RSCRIPT!" --vanilla "!ROOT_DIR!\scripts\run_app.R" "!ROOT_DIR!"

if errorlevel 1 (
  echo.
  echo [錯誤] 啟動失敗。
  echo.
  pause
  exit /b 1
)

endlocal
exit /b 0
