{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : W
Description : Appmenu Registrar DBus service
Copyright   : (c) Kasperi Kuuskoski, 2020
License     : GPL-3
Maintainer  : sixrapid@github

This code can be used alongside appmenu-gtk-module to
export application menus using dbusmenu. Can be directly
used as (for example) a systemd service. 
-}

module AppmenuRegistrar where

import           Data.Word                      ( Word32 )
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                , insert
                                                , empty
                                                , delete
                                                )
import           DBus                           
import           DBus.Client                                                
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.MVar
import           System.Posix.Signals           ( Handler(CatchOnce)
                                                , installHandler
                                                , sigINT
                                                , sigTERM
                                                )

-- |Windows are stored in a map with the id as the key and the bus and path as 
-- values. The map is further wrapped in an MVar to accommodate the 
-- event-callback construction of DBus interfaces.
newtype WindowMap = WindowMap (MVar (Map Word32 (BusName, ObjectPath)))

newWindowMap :: IO WindowMap
newWindowMap = WindowMap <$> newMVar empty

printWindowMap :: WindowMap -> IO ()
printWindowMap (WindowMap ref) = readMVar ref >>= print


-- |The following three methods are the ones which will be exported to the
-- interface.
registerWindow :: WindowMap -> MethodCall -> Word32 -> ObjectPath -> IO ()
registerWindow (WindowMap ref) call wid path = case methodCallSender call of
  Just bus -> print ("Registered window with ID " <> show wid)
    >> modifyMVar_ ref (return . insert wid (bus, path))
  Nothing -> return ()

unregisterWindow :: WindowMap -> Word32 -> IO ()
unregisterWindow (WindowMap ref) wid =
  print ("Unregistered window with ID " <> show wid)
    >> modifyMVar_ ref (return . delete wid)

getWindowById :: WindowMap -> Word32 -> IO (Either Reply (String, ObjectPath))
getWindowById (WindowMap ref) wid = do
  wm <- readMVar ref
  case wm !? wid of
    Just (bus, path) -> return $ Right (formatBusName bus, path)
    Nothing          -> return . Left $ ReplyError
      (errorName_ "com.canonical.AppMenu.Registrar.ItemNotFound")
      []


-- | Export methods for Appmenu Registrar DBus-interface.
exportMethods :: Client -> WindowMap -> IO ()
exportMethods client wmap = export
  client
  "/com/canonical/AppMenu/Registrar"
  defaultInterface
    { interfaceName    = "com.canonical.AppMenu.Registrar"
    , interfaceMethods = [ autoMethodWithMsg "RegisterWindow"
                           $ registerWindow wmap
                         , autoMethod "UnregisterWindow" $ unregisterWindow wmap
                         , autoMethod "GetMenuForWindow" $ getWindowById wmap
                         ]
    }


-- | If something bad happens, fill token
handler :: MVar () -> Handler
handler token = CatchOnce $ putMVar token ()

-- | Loop - if token is empty, everything fine, continue as usual. If token is
-- not empty, disconnect and terminate.
loop :: MVar () -> Client -> IO ()
loop v client = do
  val <- tryTakeMVar v
  case val of
    Just _  -> print "Appmenu Registrar terminated." >> disconnect client
    Nothing -> threadDelay 1000000 >> loop v client

-- | Main loop
main :: IO ()
main = do
  wmap  <- newWindowMap
  token <- newEmptyMVar
  installHandler sigINT  (handler token) Nothing
  installHandler sigTERM (handler token) Nothing

  client <- connectSession
  reply  <- requestName client
                        "com.canonical.AppMenu.Registrar"
                        [nameAllowReplacement, nameReplaceExisting]

  print "Appmenu Registrar starting..."

  case reply of
    NamePrimaryOwner -> exportMethods client wmap
    NameAlreadyOwner -> exportMethods client wmap
    _                -> print "Registrar already exists!" >> putMVar token ()

  loop token client






