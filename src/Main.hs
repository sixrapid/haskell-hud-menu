{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      :  <File name or $Header$ to be replaced automatically>
Description :  HUD-menu for rofi
Copyright   :  (c) Kasperi Kuuskoski
License     :  GPL 3.0?

Maintainer  :  sixrapid@github
Stability   :  unstable
Portability :  portable

<module description starting at first column>
-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import qualified Data.Map.Ordered as OM

import GHC.Exts
import Data.Int (Int32)
import Data.Maybe
import DBus
import DBus.Client
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import System.Process
import System.IO (hClose, hWaitForInput)

-- constants
emspace :: T.Text
patharrow :: T.Text
pathseparator :: T.Text

emspace       = "\x2003"
patharrow     = "\x00BB"
pathseparator = emspace <> patharrow <> emspace


-- |Get the id for the currently active window from Xlib
getActiveWindowId :: IO (Maybe Int32)
getActiveWindowId = do
  dpy <- openDisplay ""   -- open display from X server
  rootw <- rootWindow dpy (defaultScreen dpy)   -- get the root window
  atom <- internAtom dpy "_NET_ACTIVE_WINDOW" True    -- get atom which has information of active window
  prop <- getWindowProperty32 dpy atom rootw    -- get the property that includes the active window id
-- return $ fromIntegral $ head $ fromJust prop   -- format the property to Integer and return
  return $ fmap (fromIntegral . head) prop


-- | This function retrieves the menu for a window by id
-- using the dbusmenu and appmenu registrar interfaces
retrieveDbusmenu :: Int32 -> IO [Variant]
retrieveDbusmenu id = do
  client <- connectSession

  -- two dbus queries to get the menu
  -- 1) query appmenu registrar for the dbusmenu path and bus specific to the active window
  -- 2) query dbusmenu for the menu using the identifiers from step 1

  -- query 1
  let registrarPath = objectPath_ "/com/canonical/AppMenu/Registrar"
  let registrarIf = interfaceName_ "com.canonical.AppMenu.Registrar"
  let registrarCall = memberName_ "GetMenuForWindow"
  let registrarBus = Just . busName_ $ "com.canonical.AppMenu.Registrar"

  registrarReply <- call_ client (methodCall registrarPath registrarIf registrarCall)
    { methodCallDestination = registrarBus, methodCallBody = [toVariant id] }

  -- call returns (bus, path)
  let [fst, snd] = methodReturnBody registrarReply

  -- SWITCH OVER TO GI-DBUSMENU?
  -- query 2
  let dbusmenuPath :: ObjectPath = fromJust . fromVariant $ snd
  let dbusmenuIf = interfaceName_ "com.canonical.dbusmenu"
  let dbusmenuCall = memberName_ "GetLayout"
  let dbusmenuBus :: Maybe BusName = fromVariant fst

  dbusmenuReply <- call_ client (methodCall dbusmenuPath dbusmenuIf dbusmenuCall)
    { methodCallDestination = dbusmenuBus
    , methodCallBody = [toVariant (0 :: Int32),             -- parentId = 0 -> gives the layout of the root
                        toVariant (-1 :: Int32),            -- recursionDepth = -1 -> gives every item (recursively) under the root
                        toVariant (["label"] :: [T.Text])]  -- propertyNames -> we only care about the id and the label of each item
    }

  -- dbusmenuReply is of format [some number?, [(root id, {"label": root label}, children)]] where
  -- children = [(child id, {"label": child label}, children)] and so on. notable are the quite useless
  -- lists that surround each tuple.

  return . tail . methodReturnBody $ dbusmenuReply

-- problem with implementation - see https://github.com/electron/electron/issues/8297
-- have to rewrite a lot of stuff...


-- |This function formats the dbusmenu reply to an ordered map with
-- formatted path as keys and id's as values.
formatDbusmenu :: [Variant] -> OM.OMap T.Text Int32
formatDbusmenu = explore' ""
  where explore' :: T.Text -> [Variant] -> OM.OMap T.Text Int32
        explore' path ((tuplify -> (id, label, children)) : neighbors) 
          | label == ""   = explore' path neighbors
          | null children = (newpath, id) OM.<| explore' path neighbors
          | otherwise     = explore' newpath children OM.|<> explore' path neighbors
          where newpath
                  | label == "Root" = ""
                  | null children   = path <> T.replace "_" "" label
                  | otherwise       = path <> T.replace "_" "" label <> pathseparator
        explore' p [] = OM.empty


tuplify :: Variant -> (Int32, T.Text, [Variant])
tuplify v = (id, label, children) where
  (id, getLabel -> label, children) = fromJust . fromVariant $ v

getLabel :: M.Map T.Text Variant -> T.Text
getLabel [("label",v)] = fromJust . fromVariant $ v
getLabel [] = ""


writeToRofi :: OM.OMap T.Text Int32 -> IO T.Text
writeToRofi menu = do
  (Just hin, Just hout, _, _) <- createProcess (proc "rofi" ["-dmenu", "-i", "-p", "HUD"])
    { std_in = CreatePipe, std_out = CreatePipe }

  let keys = foldr (\(f,s) a -> f:a) [] (OM.assocs menu)
  TIO.hPutStr hin (T.intercalate "\n" keys)
  hClose hin

  output <- TIO.hGetLine hout
  hClose hout

  return output

  
-- data Node = Node Int32 String [Node]

-- tryGtkInterface :: undefined

main :: IO ()
main = do
  id <- getActiveWindowId
  reply <- retrieveDbusmenu . fromJust $ id
  choice <- writeToRofi . formatDbusmenu $ reply
  TIO.putStrLn choice