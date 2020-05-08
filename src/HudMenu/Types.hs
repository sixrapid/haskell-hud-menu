module HudMenu.Types where


import           Control.Applicative
import           Control.Monad.Trans.Maybe      ( MaybeT )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Data.Int                       ( Int32 )
import           Data.Map.Ordered.Strict        ( OMap )
import           Data.Text                      ( Text )
import           DBus.Client                    ( Client )
import           DBus.Internal.Types


data MenuAddress  = MenuAddress BusName ObjectPath
  deriving (Eq, Show)
data Item         = Item { itemId :: ItemId, itemLabel :: ItemLabel}
  deriving (Eq, Show)

data SupportFlag = SupportsDbusmenu | SupportsGtkMenus | NotSupported
  deriving (Eq, Show)

instance Semigroup SupportFlag where
  a <> b = a

instance Monoid SupportFlag where
  mempty = NotSupported


type MaybeDBusR = MaybeT (ReaderT Client IO)
type WindowId = Int32
type ItemId = Int32
type ItemLabel = Text
type MenuMap = OMap Text ItemId


-- Separator for menu items.
sep   :: Text
sep   = em <> arrow <> em
  where
    em    = "\x2003"
    arrow = "\x00BB"

