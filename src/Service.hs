{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Service where

import           DBus
import           DBus.Client

import           Data.IORef
import           Data.Maybe
import           Data.Word            (Word32)

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (forever)
import           Control.Monad.Reader (ReaderT (..))

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M

--import qualified GI.GLib as GLib

daemon :: IORef (Map Word32 (BusName, ObjectPath)) -> IO()
daemon windowMap = do
  client <- connectSession

  -- add unregisterwindow?

  let registerWindow = makeMethod name inSig outSig fun where
        name = "RegisterWindow"
        inSig = signature_ [TypeWord32, TypeObjectPath]
        outSig = signature_ []
        fun :: MethodCall -> ReaderT Client IO Reply -- DbusR a = ReaderT Client IO a (annoying alias)
        fun call = ReaderT \client ->                -- ReaderT constructor: ReaderT (Client -> IOReply)
          modifyIORef' windowMap (M.insert wid (sender, path)) >> return (ReplyReturn [])
            where
              [fromJust . fromVariant -> wid , fromJust . fromVariant -> path] = methodCallBody call
              sender = fromJust . methodCallSender $ call

  let getMenuForWindow = makeMethod name inSig outSig fun where
        name = memberName_ "GetMenuForWindow"
        inSig = signature_ [TypeWord32]
        outSig = signature_ [TypeString, TypeObjectPath]
        fun :: MethodCall -> ReaderT Client IO Reply
        fun (map (fromJust . fromVariant) . methodCallBody -> [wid]) =
          ReaderT \client -> do
            map <- readIORef windowMap
            return . ReplyReturn $ [toVariant . fromJust $ M.lookup wid map]

  _ <- requestName client "com.canonical.AppMenu.Registrar" [nameAllowReplacement, nameReplaceExisting]

  export client "/com/canonical/AppMenu/Registrar" defaultInterface
    {interfaceName = "com.canonical.AppMenu.Registrar", interfaceMethods = [ registerWindow, getMenuForWindow ]}

--  forever do

main :: IO()
main = do
  windowMap <- newIORef M.empty
  daemon windowMap
  forever do
    readIORef windowMap >>= print
    threadDelay 0


