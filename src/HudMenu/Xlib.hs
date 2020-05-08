module HudMenu.Xlib where

import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras
import           HudMenu.Types

-- | Get the id for the currently active window from Xlib. If no active window
-- exists terminate the program (would happen again).
getActiveWindowId :: IO WindowId
getActiveWindowId = do
  dpy       <- openDisplay ""
  rootw     <- rootWindow dpy (defaultScreen dpy)
  atom      <- internAtom dpy "_NET_ACTIVE_WINDOW" True
  maybeProp <- getWindowProperty32 dpy atom rootw
  case maybeProp of
    Just [prop] -> return . fromIntegral $ prop
    Nothing ->
      error "ERROR: Failed to fetch active window from Xlib - terminating."
