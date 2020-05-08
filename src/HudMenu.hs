{-# LANGUAGE OverloadedStrings   #-}

module HudMenu where
          
import           Control.Monad.IO.Class        ( liftIO )
import           Control.Monad.Loops           ( whileM_ )
import           Control.Monad.Trans.Reader    ( ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Maybe     ( MaybeT(..) )

import           Data.Int                      ( Int32 )
import           Data.Map.Strict               ( Map, (!?) )
import qualified Data.Map.Strict               as M
import           Data.Map.Ordered.Strict       ( OMap, (|<>), (|<) )
import qualified Data.Map.Ordered.Strict       as OM
import           Data.Maybe                    ( fromMaybe )
import           Data.Text                     ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           DBus
import           DBus.Client
import           DBus.Internal.Message

import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras

import           System.IO
import           System.Process


-- | Constants
em    :: Text
arrow :: Text
sep   :: Text

em    = "\x2003"
arrow = "\x00BB"
sep   = em <> arrow <> em


-- | Custom types

data MyNode       = MyNode ItemId Text [Variant] -- ^ no support for multiple props
data MenuAddress  = MenuAddress BusName ObjectPath

type Stack    = MaybeT (ReaderT Client IO)
type WindowId = Int32
type ItemId   = Int32


-- | Get the id for the currently active window from Xlib.
getActiveWindowId :: Stack WindowId
getActiveWindowId = do
  prop <- liftIO $ do
    dpy   <- openDisplay ""
    rootw <- rootWindow dpy (defaultScreen dpy)
    atom  <- internAtom dpy "_NET_ACTIVE_WINDOW" True
    getWindowProperty32 dpy atom rootw
  MaybeT . pure . fmap (fromIntegral . head) $ prop


-- | Helper to keep the DBus calls concise.
call' :: ObjectPath
      -> InterfaceName
      -> MemberName
      -> BusName
      -> [Variant]
      -> Stack [Variant]
call' path interface method bus body = MaybeT $ do
  client <- ask
  reply <- liftIO $ call client (methodCall path interface method)
    { methodCallDestination = Just bus
    , methodCallBody = body
    }
  
  case reply of
    Left  MethodError  {}   -> return Nothing
    Right MethodReturn {methodReturnBody = body} -> return . Just $ body


-- | Following four functions are just calls to the respective dbusmenu
-- functions - refer to com.canonical.dbusmenu documentation for more info.
getMenuForWindow :: WindowId -> Stack MenuAddress
getMenuForWindow winId = do
  [bus, path] <- call' "/com/canonical/AppMenu/Registrar"
                 "com.canonical.AppMenu.Registrar"
                 "GetMenuForWindow"
                 "com.canonical.AppMenu.Registrar"
                 [toVariant winId]
  MaybeT . return $ MenuAddress <$> fromVariant bus <*> fromVariant path


aboutToShow :: MenuAddress -> ItemId -> Stack Bool
aboutToShow (MenuAddress bus path) menuId = do
  [bool] <- call' path
            "com.canonical.dbusmenu"
            "AboutToShow"
            bus
            [toVariant menuId]
  MaybeT . return . fromVariant $ bool


getLayout :: MenuAddress -> ItemId -> Stack [Variant]
getLayout (MenuAddress bus path) menuId = do
  (rev:layout) <- call' path
                  "com.canonical.dbusmenu"
                  "GetLayout"
                  bus
                  [ toVariant menuId
                  , toVariant (-1 :: Int32)  
                  , toVariant (["label"] :: [Text])
                  ]
  return layout

--event :: ItemId -> Text -> ??? -> ???
--event = undefined


-- | documentation
getMenu :: MenuAddress -> Stack (OMap Text ItemId)
getMenu addr = recurse "" =<< getLayout addr 0
  where 
    recurse :: Text -> [Variant] -> Stack (OMap Text ItemId)
    recurse text (v:vs) =
      case nodify v of
        Nothing -> recurse text vs
        Just (MyNode id label children) -> do
          let newtext | label == "Root" = ""
                      | null children   = text <> T.replace "_" "" label
                      | otherwise       = text <> T.replace "_" "" label <> sep

          let entry = (newtext, id)
          liftIO $ print entry
          goDeep <- recurse newtext children
          goWide <- recurse text vs

          let action | T.null label  = goWide
                     | null children = entry |< goWide
                     | otherwise     = goDeep |<> goWide
          
          return action

    recurse text [] = return OM.empty

    nodify :: Variant -> Maybe MyNode
    nodify v = do
      (id, props, children) <- fromVariant v
      label <- fromVariant =<< props !? ("label" :: Text)
      return $ MyNode id label children


-- | Open rofi, pipe the given map to stdin, return stdout.
writeToRofi :: OMap Text ItemId -> IO Text
writeToRofi menu = do
  (Just hin, Just hout, _, _) <- createProcess
    (shell "rofi -dmenu -i -p HUD") { std_in  = CreatePipe
                                    , std_out = CreatePipe
                                    }

  T.hPutStr hin . T.unlines . map fst . OM.assocs $ menu
  hClose hin

  output <- T.hGetLine hout
  hClose hout

  return output


main :: IO()
main = do
  hSetBuffering stdin NoBuffering
  client <- connectSession

  let continue = putStrLn "\"q\" to quit, anything else shows the menu"
        >>  getChar
        >>= \char -> return (char /= 'q')

  whileM_ continue $ do
    maybeMap <- runReaderT (runMaybeT $ do
      wid <- getActiveWindowId
      addr <- getMenuForWindow wid
      getMenu addr) client

    case maybeMap of
      Just map -> writeToRofi map >>= T.putStrLn
      Nothing  -> T.putStrLn "wituiks män"