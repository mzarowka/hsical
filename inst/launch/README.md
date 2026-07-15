# Launching hsical on the rig PC

A one-double-click launcher: it starts the Shiny server and opens the app in a
clean browser window — no tabs, no address bar, no menus.

## Prerequisites (on the rig)

- **R** installed (any recent version, under `C:\Program Files\R\...`).
- **hsical** installed into that R, so `hsical::run_app()` works:
  ```r
  # install.packages("remotes")
  remotes::install_github("<your-org>/hsical")   # or install from a local copy
  ```
- **Microsoft Edge** or **Google Chrome** (either; Edge ships with Windows 11).
  Without one, it falls back to the default browser — which *will* show tabs.

## Set up the shortcut (once)

Copy this `launch` folder to the rig (or use the installed copy at
`system.file("launch", package = "hsical")`), then run:

```powershell
powershell -ExecutionPolicy Bypass -File Install-HsicalShortcut.ps1
```

That drops an **hsical** shortcut on the Desktop, set to run minimized.

## Daily use

Double-click **hsical**. You get:

- a minimized **hsical server** console in the taskbar (the running R process), and
- a clean **app window** with the tool in it.

Click it again later and it reuses the already-running server — it won't start a
second one.

## Stopping / restarting

- Closing the app window leaves the server running (so reopening is instant).
- To stop the server, close the minimized **hsical server** console, or end the
  `Rscript.exe` task. A reboot clears it too.

## Tweaks

Edit `hsical.cmd`:

- **Port** — change `set "PORT=7070"` (both the launcher and the browser use it).
- **Fullscreen kiosk** instead of a windowed app — swap `--app=%URL%` for
  `--kiosk %URL%` in the Edge/Chrome lines. Kiosk fills the screen with no window
  controls (Alt+F4 to exit); better only on a dedicated rig.
- **Custom icon** — set `IconLocation` in `Install-HsicalShortcut.ps1` to a `.ico`.
