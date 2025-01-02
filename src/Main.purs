module Main where

import Prelude

import Control.Alternative ((<|>))
import Deku.DOM.Listeners as DL
import Control.Monad.ST.Class (liftST)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Foldable (class Foldable, foldrDefault, foldlDefault)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, sequenceDefault)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.Hooks ((<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (Event, create)
import FRP.Poll (Poll, sham)
import Routing.Hash (matches)
import Routing.Match (Match, lit)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventListener)
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS

data Move = NW | N | NE | W | C | E | SW | S | SE 

moves :: Array Move
moves = [NW , N , NE , W , C , E , SW , S , SE ] 

instance showMove :: Show Move where
  show NW = "NW"
  show N = "N"
  show NE = "NE"
  show W = "W"
  show C = "C"
  show E = "E"
  show SW = "SW"
  show S = "S"
  show SE = "SE"

instance encodeMove :: EncodeJson Move where
  encodeJson = Json.fromString <<< show

moveLens :: Move -> BoardLens
moveLens = unsafeCoerce unit

data NoughtsRoute = NoughtRoute | CrossRoute

noughtsRoute :: Match NoughtsRoute
noughtsRoute = (NoughtRoute <$ lit "o") <|> (CrossRoute <$ lit "x")

main :: Effect Unit
main = do 
  u <- matches noughtsRoute \_ newRoute -> case newRoute of
    NoughtRoute -> play Nought
    CrossRoute -> play Cross
  u

play :: Mark -> Effect Unit
play mark = do
  conn <- WS.create ("http://localhost:8080" <> case mark of
    Nought -> "/join/O" 
    Cross -> "/join/X") []
  { push, event } <- liftST create
  let _ = pollGame event
  EET.addEventListener WSET.onMessage (listener push) false (WS.toEventTarget conn)
  void $ runInBody Deku.do
    D.div [ DA.klass_ "p-6 bg-white rounded-lg shadow-lg" ]
      [ D.h1 [ DA.klass_ "text-2xl font-bold text-center mb-4" ] [ text_ "Noughts and Crosses" ]
      , D.div [ DA.klass_ "grid grid-cols-3 gap-2 w-64 mx-auto" ] (moves <#> \move -> squareDiv (setSquare move conn) (pollSquare move conn))
      ]

listener :: (Game -> Effect Unit) -> EventListener
listener _ = unsafeCoerce unit

pollGame :: Event Game -> Poll Game
pollGame = sham

sendJSON :: WebSocket -> Json -> Effect Unit
sendJSON = unsafeCoerce unit

setSquare :: Move -> WebSocket -> Effect Unit
setSquare move conn = sendJSON conn $ encodeJson move

pollSquare :: Move -> WebSocket -> Poll Square
pollSquare = unsafeCoerce unit

type BoardLens = forall a. Lens' (Board a) a 

data Square = Active | Inactive {mark :: Maybe Mark, won :: Boolean}


squareDiv :: Effect Unit -> Poll Square -> Nut
squareDiv set poll = poll <#~> case _ of 
  Active -> activeSquare
  Inactive {mark, won} -> inactiveSquare mark won
  where
  activeSquare = D.div
      [ klass
      , DL.click_ $ \_ -> set
      ] [ text_ "-" ]
  inactiveSquare mark won = D.div
      ([ klass ] <>
      case won of
        true -> [DA.style_ "color:red"]
        false -> []
      )
      [ case mark of
          Just Cross -> text_ "X"
          Just Nought -> text_ "O"
          Nothing -> text_ "-"
      ]
  klass = DA.klass_ "cell flex items-center justify-center w-20 h-20 bg-gray-200 text-3xl font-bold rounded cursor-pointer hover:bg-gray-300"
--  ( D.div
--      [ DA.klass_ "cell flex items-center justify-center w-20 h-20 bg-gray-200 text-3xl font-bold rounded cursor-pointer hover:bg-gray-300" , DL.runOn DL.click $ poll <#> (setS <<< f)
--      , DA.style $ filter identity w $> "color:red"
--      , DA.unset @"style" $ filter not w
--      ]
--      [ poll <#~> \{board} -> case view lens board of
--          Just Cross -> text_ "X"
--          Just Nought -> text_ "O"
--          Nothing -> text_ "-"
--      ]
--  )
--  where
--  f game = fromMaybe game $ updateBoard lens game 
--  w = poll <#> \{status} -> winningSquare lens status


getDisj :: Disj Boolean -> Boolean
getDisj (Disj b) = b


_nw :: BoardLens
_nw = _Newtype <<< prop (Proxy :: Proxy "nw")

_n :: BoardLens
_n = _Newtype <<< prop (Proxy :: Proxy "n")

_ne :: BoardLens
_ne = _Newtype <<< prop (Proxy :: Proxy "ne")

_w :: BoardLens
_w = _Newtype <<< prop (Proxy :: Proxy "w")

_c :: BoardLens
_c = _Newtype <<< prop (Proxy :: Proxy "c")

_e :: BoardLens
_e = _Newtype <<< prop (Proxy :: Proxy "e")

_sw :: BoardLens
_sw = _Newtype <<< prop (Proxy :: Proxy "sw")

_s :: BoardLens
_s = _Newtype <<< prop (Proxy :: Proxy "s")

_se :: BoardLens
_se = _Newtype <<< prop (Proxy :: Proxy "se")

data Mark = Nought | Cross
type Space = Maybe Mark

type Game = { status :: Status, board :: Board Space }

startingGame :: Game
startingGame = { status: Playing Nought , board: emptyBoard }

data Status = Finished (Board (Disj Boolean)) | Playing Mark

newtype Board a = Board
  { nw :: a
  , n :: a
  , ne :: a
  , w :: a
  , c :: a
  , e :: a
  , sw :: a
  , s :: a
  , se :: a
  }

derive instance Newtype (Board a) _

emptyBoard :: Board Space
emptyBoard = Board
  { nw: Nothing
  , n: Nothing
  , ne: Nothing
  , w: Nothing
  , c: Nothing
  , e: Nothing
  , sw: Nothing
  , s: Nothing
  , se: Nothing
  }

instance functorBoard :: Functor Board where 
  map f (Board {nw, n, ne, w, c, e, sw, s, se}) = Board {nw: f nw, n: f n, ne: f ne, w: f w, c: f c, e: f e, sw: f sw, s: f s, se: f se}

instance foldableBoard :: Foldable Board where
  foldMap f (Board {nw, n, ne, w, c, e, sw, s, se}) = f nw <> f n <> f ne <> f w <> f c <> f e <> f sw <> f s <> f se
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance traverseBoard :: Traversable Board where
  traverse f (Board {nw, n, ne, w, c, e, sw, s, se}) = map Board $ {nw: _, n: _, ne: _, w: _, c: _, e: _, sw: _, s: _, se: _} <$> f nw <*> f n <*> f ne <*> f w <*> f c <*> f e <*> f sw <*> f s <*> f se 
  sequence = sequenceDefault

