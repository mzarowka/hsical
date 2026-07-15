@echo off
rem ===========================================================================
rem hsical launcher — starts the Shiny server (once) and opens it in a
rem chrome-less browser window. Double-click, or point a Desktop shortcut here
rem (see Install-HsicalShortcut.ps1). Idempotent: if the server is already
rem running on %PORT%, it just opens another clean window against it.
rem ===========================================================================
setlocal enableextensions enabledelayedexpansion

set "PORT=7070"
set "URL=http://127.0.0.1:%PORT%/"
set "PROFILE=%TEMP%\hsical-browser"

rem --- locate Rscript.exe: newest R under Program Files, else PATH ------------
set "RSCRIPT="
for /f "delims=" %%D in ('dir /b /ad /o-n "%ProgramFiles%\R\R-*" 2^>nul') do (
  if not defined RSCRIPT if exist "%ProgramFiles%\R\%%D\bin\Rscript.exe" (
    set "RSCRIPT=%ProgramFiles%\R\%%D\bin\Rscript.exe"
  )
)
if not defined RSCRIPT where Rscript >nul 2>nul && set "RSCRIPT=Rscript"
if not defined RSCRIPT (
  echo Could not find Rscript.exe. Install R, or add its bin folder to PATH.
  pause & exit /b 1
)

rem --- start the server only if the port isn't already listening -------------
call :port_up && goto :wait
start "hsical server" /min "%RSCRIPT%" -e "hsical::run_app(port=%PORT%, host='127.0.0.1', launch.browser=FALSE)"

rem --- wait (up to ~60s) for the server to accept connections ----------------
:wait
for /l %%i in (1,1,60) do (
  call :port_up && goto :open
  timeout /t 1 >nul
)
echo Server did not come up on port %PORT% in time.
pause & exit /b 1

rem --- open a clean app window: Edge first, then Chrome, then default --------
:open
set "EDGE=%ProgramFiles(x86)%\Microsoft\Edge\Application\msedge.exe"
if not exist "%EDGE%" set "EDGE=%ProgramFiles%\Microsoft\Edge\Application\msedge.exe"
if exist "%EDGE%" (
  start "" "%EDGE%" --app=%URL% --user-data-dir="%PROFILE%" --no-first-run --no-default-browser-check
  exit /b 0
)
set "CHROME=%ProgramFiles%\Google\Chrome\Application\chrome.exe"
if not exist "%CHROME%" set "CHROME=%ProgramFiles(x86)%\Google\Chrome\Application\chrome.exe"
if exist "%CHROME%" (
  start "" "%CHROME%" --app=%URL% --user-data-dir="%PROFILE%" --no-first-run --no-default-browser-check
  exit /b 0
)
rem Fallback: default browser (this one WILL have tabs and an address bar).
start "" "%URL%"
exit /b 0

rem --- helper: succeed (errorlevel 0) if something is listening on %PORT% ----
:port_up
powershell -NoProfile -Command "try{$c=New-Object Net.Sockets.TcpClient;$c.Connect('127.0.0.1',%PORT%);$c.Close();exit 0}catch{exit 1}"
exit /b %errorlevel%
