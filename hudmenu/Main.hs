{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  ( unless )

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Data.Map.Ordered.Strict        ( OMap )
import qualified Data.Map.Ordered.Strict       as M

import           DBus
import           DBus.Client

import           System.IO
import           System.Process

import           HudMenu.Types
import           HudMenu.Xlib
import           HudMenu.Dbusmenu

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  client  <- connectSession
  winId   <- getActiveWindowId

  -- support <- (tryDbusmenu wid client) <> tryGtkMenus 
  support <- tryDbusmenu winId client

  unless (support == NotSupported) $ do
    (Just hin, Just hout, _, _) <- createProcess (shell "rofi -dmenu -i -p HUD")
      { std_in  = CreatePipe
      , std_out = CreatePipe
      }

    maybeMap <- case support of
      SupportsDbusmenu -> menuFromDbusmenu winId client
      --SupportsGtkMenus -> undefined
      _                -> return Nothing

    case maybeMap of
      Nothing -> T.putStrLn "Error" >> hClose hout >> hClose hin
      Just m  -> do
        T.hPutStr hin . T.unlines . map fst . M.assocs $ m
        hClose hin
        escape <- hIsEOF hout
        unless escape $ do
          key <- T.hGetLine hout
          case M.lookup key m of
            Nothing     -> hClose hout
            Just itemId -> sendToDbusmenu winId itemId client >> hClose hout
