# Creates a Desktop shortcut ("hsical") that runs hsical.cmd minimized, so the
# only thing the operator sees is the clean browser window (plus a minimized
# "hsical server" console in the taskbar). Run once on the rig PC:
#
#   powershell -ExecutionPolicy Bypass -File Install-HsicalShortcut.ps1
#
# Re-run any time to refresh the shortcut (e.g. after moving the folder).

$ErrorActionPreference = 'Stop'

$here    = Split-Path -Parent $MyInvocation.MyCommand.Path
$target  = Join-Path $here 'hsical.cmd'
if (-not (Test-Path $target)) { throw "hsical.cmd not found next to this script: $target" }

$desktop = [Environment]::GetFolderPath('Desktop')
$lnkPath = Join-Path $desktop 'hsical.lnk'

$shell    = New-Object -ComObject WScript.Shell
$shortcut = $shell.CreateShortcut($lnkPath)
$shortcut.TargetPath       = $target
$shortcut.WorkingDirectory = $here
$shortcut.WindowStyle      = 7      # 7 = minimized
$shortcut.Description       = 'Launch hsical in a clean browser window'
# Generic app icon from shell32; swap for a custom .ico if you have one:
$shortcut.IconLocation     = "$env:SystemRoot\System32\SHELL32.dll,13"
$shortcut.Save()

Write-Host "Created shortcut: $lnkPath"
Write-Host "Target:           $target"
