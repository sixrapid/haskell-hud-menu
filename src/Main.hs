{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

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

import           Data.Map.Ordered         (OMap)
import qualified Data.Map.Ordered         as OM

import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M

import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

import           Data.Int                 (Int32)
import           Data.Maybe               (fromJust)

import           Control.Monad.Loops
import           Control.Monad.Trans.Maybe 
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class   (liftIO)

import           System.IO
import           System.Process

import           DBus
import           DBus.Client
import           DBus.Internal.Message

import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras


-- | Constants.
emspace :: Text
patharrow :: Text
pathseparator :: Text

emspace       = "\x2003"
patharrow     = "\x00BB"
pathseparator = emspace <> patharrow <> emspace


-- | Helper function to keep the dbus calls from taking up too much space.
call' 
  :: Client 
  -> ObjectPath
  -> InterfaceName
  -> MemberName
  -> BusName
  -> [Variant]
  -> IO (Either MethodError MethodReturn)
call' client path interf method dest body = 
  call client (methodCall path interf method) 
    { methodCallDestination = Just dest
    , methodCallBody = body
    }


-- | Get the id for the currently active window from Xlib.
getActiveWindowId :: MaybeT IO Int32
getActiveWindowId = do
  prop <- liftIO $ do dpy   <- openDisplay ""
                      rootw <- rootWindow dpy (defaultScreen dpy)
                      atom  <- internAtom dpy "_NET_ACTIVE_WINDOW" True
                      getWindowProperty32 dpy atom rootw
  MaybeT . pure . fmap (fromIntegral . head) $ prop


-- | Call appmenu registrar for the bus and path of the window with @id@
callRegistrar :: Client -> Int32 -> MaybeT IO (BusName, ObjectPath)
callRegistrar client id = do
  reply <- liftIO $ call' client 
                          "/com/canonical/AppMenu/Registrar"
                          "com.canonical.AppMenu.Registrar"
                          "GetMenuForWindow"
                          "com.canonical.AppMenu.Registrar"
                          [toVariant id]

  MaybeT . pure $ formatReply reply where
    formatReply (Left  MethodError  {..}) = Nothing
    formatReply (Right MethodReturn {..}) = tuplify <$> fromVariant a <*> fromVariant b where
      [a,b]       = methodReturnBody
      tuplify a b = (a,b)


-- | Call dbusmenu for the menu of the window with @bus@ and @path@.
-- parentId determines the menuitem whose children dbusmenu returns.
-- depth determines the recursion depth.
-- props is a list of the properties that dbusmenu should return for each menuitem.
-- Returns a @[Variant]@ with layout [(id, props, children), (id, props, children), ...],
-- where @id :: Int32@, @props :: Map Text Text@ and @children :: [Variant]@.
callDbusmenu :: Client -> BusName -> ObjectPath -> Int32 -> Int32 -> [Text] -> MaybeT IO [Variant]
callDbusmenu client bus path parentId depth props = do
  reply <- liftIO $ call' client
                          path
                          "com.canonical.dbusmenu"
                          "GetLayout"
                          bus
                          [ toVariant (parentId :: Int32)
                          , toVariant (depth :: Int32)
                          , toVariant (props :: [Text])
                          ]

  MaybeT . pure $ formatReply reply where
    formatReply (Left MethodError {..})   = Nothing
    formatReply (Right MethodReturn {..}) = Just . tail $ methodReturnBody  


-- | Format the reply from dbusmenu to an ordered map.
-- The formatted paths act as keys and the window ids as values.
-- TODO: rewrite - see https://github.com/electron/electron/issues/8297
explore :: [Variant] -> OMap Text Int32
explore = explore' ""
  where explore' :: Text -> [Variant] -> OMap Text Int32
        explore' path ((tuplify -> (id, label, children)) : neighbors)
          | label == ""   = explore' path neighbors
          | null children = (newpath, id) OM.|< explore' path neighbors
          | otherwise     = explore' newpath children OM.|<> explore' path neighbors
          where newpath
                  | label == "Root" = ""
                  | null children   = path <> T.replace "_" "" label
                  | otherwise       = path <> T.replace "_" "" label <> pathseparator
        explore' p [] = OM.empty

-- TODO: Better error handling
tuplify :: Variant -> (Int32, Text, [Variant])
tuplify v = (id, label, children) where
  (id, getLabel -> label, children) = fromJust . fromVariant $ v
  
-- TODO: Better error handling
getLabel :: Map Text Variant -> Text
getLabel map = fromJust . fromJust $ fromVariant <$> M.lookup "label" map


-- | Open rofi, pipe the given map to stdin, return stdout.
writeToRofi :: OMap Text Int32 -> IO Text
writeToRofi menu = do
  (Just hin, Just hout, _, _) <- createProcess (proc "rofi" ["-dmenu", "-i", "-p", "HUD"])
    { std_in = CreatePipe, std_out = CreatePipe }

  T.hPutStr hin . T.unlines . map fst . OM.assocs $ menu
  hClose hin

  output <- T.hGetLine hout
  hClose hout

  return output


-- tryGtkInterface :: undefined


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  client <- connectSession

  let ask = putStrLn "\"q\" to quit, anything else shows the menu" >>
        getChar >>= \char -> 
          return (char /= 'q')
  
  maybeMenu <- runMaybeT $ do
    id <- getActiveWindowId
    (bus, path) <- callRegistrar client id
    callDbusmenu client bus path 0 (-1) ["label"]

  case maybeMenu of
    Just map -> whileM_ ask ((writeToRofi . explore $ map) >>= T.putStrLn)
    Nothing -> T.putStrLn "voi vittu"