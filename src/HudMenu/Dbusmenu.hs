module HudMenu.Dbusmenu
  ( tryDbusmenu
  , menuFromDbusmenu
  , sendToDbusmenu
  )
where

import           Control.Monad                  ( foldM )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                , runReaderT
                                                )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )

import           Data.Int                       ( Int32 )
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                )
import qualified Data.Map.Strict               as M
import           Data.Map.Ordered.Strict        ( (>|)
                                                , OMap
                                                , (|<>)
                                                , (|<)
                                                )
import qualified Data.Map.Ordered.Strict       as OM
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Tree                      ( Tree(..)
                                                , unfoldTreeM_BF
                                                )
import           Data.Word                      ( Word32 )

import           DBus
import           DBus.Client
import           DBus.Internal.Message          ( MethodError(..)
                                                , MethodReturn(..)
                                                )
import           DBus.Internal.Types            ( Variant(..)
                                                , Value(..)
                                                )

import           HudMenu.Types



-- | Helper to keep the DBus calls concise.
call'
  :: ObjectPath
  -> InterfaceName
  -> MemberName
  -> BusName
  -> [Variant]
  -> MaybeDBusR [Variant]
call' path interface method dest body = MaybeT $ do
  client <- ask
  reply  <- liftIO $ call
    client
    (methodCall path interface method) { methodCallDestination = Just dest
                                       , methodCallBody        = body
                                       }
  case reply of
    Left  a -> return Nothing
    Right MethodReturn { methodReturnBody = body } -> return . Just $ body


-- | Following four functions are just calls to the respective dbusmenu
-- functions - refer to com.canonical.dbusmenu documentation for more info.
getMenuForWindow :: WindowId -> MaybeDBusR MenuAddress
getMenuForWindow winId = do
  [bus, path] <- call' "/com/canonical/AppMenu/Registrar"
                       "com.canonical.AppMenu.Registrar"
                       "GetMenuForWindow"
                       (busName_ "com.canonical.AppMenu.Registrar")
                       [toVariant winId]
  MaybeT . return $ MenuAddress <$> fromVariant bus <*> fromVariant path


aboutToShow :: MenuAddress -> ItemId -> MaybeDBusR Bool
aboutToShow (MenuAddress bus path) itemId = do
  [bool] <- call' path
                  "com.canonical.dbusmenu"
                  "AboutToShow"
                  bus
                  [toVariant itemId]
  MaybeT . return . fromVariant $ bool


getLayout :: MenuAddress -> ItemId -> MaybeDBusR (Tree Item)
getLayout (MenuAddress bus path) itemId = do
  [rev, layout] <- call'
    path
    "com.canonical.dbusmenu"
    "GetLayout"
    bus
    [toVariant itemId, toVariant (-1 :: Int32), toVariant (["label"] :: [Text])]

  unfoldTreeM_BF
    (\v -> MaybeT . return $ do
      (id, props :: Map Text Variant, children) <- fromVariant v
      case props !? "label" of
        Just v  -> fromVariant v >>= \label -> return (Item id label, children)
        Nothing -> return (Item id "", children)
    )
    layout

event :: MenuAddress -> ItemId -> DBusR ()
event (MenuAddress bus path) itemId = do
  r <- runMaybeT $ call'
    path
    "com.canonical.dbusmenu"
    "Event"
    bus
    [ toVariant itemId
    , toVariant ("clicked" :: Text)
    , toVariant . toVariant $ (0 :: Int32)
    , toVariant (0 :: Word32)
    ]
  return ()


-- | create a map with formatted labels as keys and menuitem id's as values
-- using the getLayout function.
makeMenu :: MenuAddress -> MaybeDBusR MenuMap
makeMenu addr = do
  _    <- aboutToShow addr 0
  root <- getLayout addr 0
  foldM (f "") OM.empty (subForest root)
 where
  f :: Text -> MenuMap -> Tree Item -> MaybeDBusR MenuMap
  f text map Node { rootLabel = Item id label, subForest = children } = do
    let newText = text <> T.replace "_" "" label

    -- only add items with an action to the map, ie. do not add items without
    -- a label (visual stuff) or items with children (submenu headers).
    if T.null label || (not . null $ children)
      then foldM (f $ newText <> sep) map children
      else do
        newCh <- aboutToShow addr id >> subForest <$> getLayout addr id
        if not . null $ newCh
          then foldM (f $ newText <> sep) map newCh
          else foldM (f newText) map newCh
            >>= \rest -> return $ rest >| (newText, id)


-- EXPORTED FUNCTIONS:
-- | check if menu for this window can be retrieved using dbusmenu
tryDbusmenu :: WindowId -> Client -> IO SupportFlag
tryDbusmenu winId client = do
  maybeAddr <- runReaderT (runMaybeT $ getMenuForWindow winId) client
  case maybeAddr of
    Just addr -> return SupportsDbusmenu
    _         -> return NotSupported

-- | return map for given window id
menuFromDbusmenu :: WindowId -> Client -> IO (Maybe MenuMap)
menuFromDbusmenu wid = runReaderT . runMaybeT $ do
  addr <- getMenuForWindow wid
  makeMenu addr

-- | send selection to dbusmenu
sendToDbusmenu :: WindowId -> ItemId -> Client -> IO ()
sendToDbusmenu winId itemId = runReaderT $ do
  maybeAddr <- runMaybeT $ getMenuForWindow winId
  case maybeAddr of
    Just addr -> event addr itemId
    Nothing   -> return ()

