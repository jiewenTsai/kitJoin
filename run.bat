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

if not defined KITJOIN_PORT set "KITJOIN_PORT=7600"

echo 正在檢查相依套件並啟動 kitJoin...
echo 瀏覽器將於數秒後開啟 http://127.0.0.1:%KITJOIN_PORT%
echo.

rem R 端 launch.browser 失敗時的備援：延遲後用 Windows start 開啟
start "" cmd /c "timeout /t 4 /nobreak >nul && start http://127.0.0.1:%KITJOIN_PORT%"

"%RSCRIPT%" --vanilla "%~dp0scripts\run_app.R"

set "RC=%ERRORLEVEL%"
if not "%RC%"=="0" (
  echo.
  echo [錯誤] 啟動失敗（代碼 %RC%）。
  pause
  exit /b %RC%
)

endlocal
