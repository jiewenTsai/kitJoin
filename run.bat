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
echo 伺服器就緒後將自動開啟 http://127.0.0.1:%KITJOIN_PORT%
echo.

rem 背景等待伺服器啟動後用 Windows start 開瀏覽器（不依賴 R 的 browseURL）
start "" cmd /c "for /l %%i in (1,1,180) do (curl -fs -o nul http://127.0.0.1:%KITJOIN_PORT% 2>nul && (start http://127.0.0.1:%KITJOIN_PORT% & exit /b 0)) & timeout /t 1 /nobreak >nul"

"%RSCRIPT%" --vanilla "%~dp0scripts\run_app.R"

set "RC=%ERRORLEVEL%"
if not "%RC%"=="0" (
  echo.
  echo [錯誤] 啟動失敗（代碼 %RC%）。
  pause
  exit /b %RC%
)

endlocal
