@echo off
setlocal EnableExtensions EnableDelayedExpansion
chcp 65001 >nul 2>&1
cd /d "%~dp0"

set "RSCRIPT="
where Rscript >nul 2>&1 && set "RSCRIPT=Rscript"

if not defined RSCRIPT (
  for /f "delims=" %%V in ('dir /b /ad /o-n "C:\Program Files\R\R-*" 2^>nul') do (
    if exist "C:\Program Files\R\%%V\bin\Rscript.exe" (
      set "RSCRIPT=C:\Program Files\R\%%V\bin\Rscript.exe"
      goto have_r
    )
  )
)

:have_r
if not defined RSCRIPT (
  echo [錯誤] 找不到 R。請先安裝 R，並確認 Rscript 已加入 PATH。
  echo        https://cran.r-project.org/bin/windows/base/
  pause
  exit /b 1
)

echo 正在檢查相依套件並啟動 kitJoin...
echo.

"%RSCRIPT%" --vanilla "%~dp0scripts\run_app.R"

set "RC=%ERRORLEVEL%"
if not "%RC%"=="0" (
  echo.
  echo [錯誤] 啟動失敗（代碼 %RC%）。
  pause
  exit /b %RC%
)

endlocal
