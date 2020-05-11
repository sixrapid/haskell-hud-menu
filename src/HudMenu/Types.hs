module HudMenu.Types where


import           Control.Applicative
import           Control.Monad.Trans.Maybe      ( MaybeT )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Data.Int                       ( Int32 )
import           Data.Word                      ( Word32 )
import           Data.Map.Ordered.Strict        ( OMap )
import           Data.Text                      ( Text )
import           DBus.Client                    ( Client )
import           DBus.Internal.Types


---- GENERAL ----

-- X window id's are unsigned longs
type WindowId = Word32

-- Map of menuitems and id's ready to be written to rofi.
type MenuMap = OMap Text ItemId

-- Monoidal type for easily finding out which interface a given window supports. 
data SupportFlag = SupportsDbusmenu | SupportsGtkMenus | NotSupported
  deriving (Eq, Show)

instance Semigroup SupportFlag where
  a <> b = a

instance Monoid SupportFlag where
  mempty = NotSupported

-- Separator for menu items
sep :: Text
sep = em <> arrow <> em
 where
  em    = "\x2003"
  arrow = "\x00BB"


---- DBUSMENU RELATED ----
-- these probably could use some work...

-- Common argument in DBusmenu calls 
data MenuAddress = MenuAddress BusName ObjectPath
  deriving (Eq, Show)

-- Dbusmenu item format
type ItemId = Int32
type ItemLabel = Text
data Item = Item { itemId :: ItemId, itemLabel :: ItemLabel}
  deriving (Eq, Show)

-- Weird alias to be honest
type MaybeDBusR = MaybeT (ReaderT Client IO)






