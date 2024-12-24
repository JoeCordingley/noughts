module Main where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.ST.Class (liftST)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj(..))
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

-- import Unsafe.Coerce (unsafeCoerce)

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

type Square = {mark :: Maybe Mark, yourTurn :: Boolean}

squareDiv :: Effect Unit -> Poll Square -> Nut
squareDiv _ poll = poll <#~> case _ of 
  {mark: Nothing, yourTurn: true} -> activeSquare
  {mark} -> inactiveSquare mark
  where
  activeSquare = unsafeCoerce unit
  inactiveSquare = unsafeCoerce unit

--  ( D.div
--      [ DA.klass_ "cell flex items-center justify-center w-20 h-20 bg-gray-200 text-3xl font-bold rounded cursor-pointer hover:bg-gray-300"
--      , DL.runOn DL.click $ poll <#> (setS <<< f)
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
_nw = prop (Proxy :: Proxy "nw")

_n :: BoardLens
_n = prop (Proxy :: Proxy "n")

_ne :: BoardLens
_ne = prop (Proxy :: Proxy "ne")

_w :: BoardLens
_w = prop (Proxy :: Proxy "w")

_c :: BoardLens
_c = prop (Proxy :: Proxy "c")

_e :: BoardLens
_e = prop (Proxy :: Proxy "e")

_sw :: BoardLens
_sw = prop (Proxy :: Proxy "sw")

_s :: BoardLens
_s = prop (Proxy :: Proxy "s")

_se :: BoardLens
_se = prop (Proxy :: Proxy "se")

data Mark = Nought | Cross
type Space = Maybe Mark

type Game = { status :: Status, board :: Board Space }

startingGame :: Game
startingGame = { status: Playing Nought , board: emptyBoard }

data Status = Finished (Board (Disj Boolean)) | Playing Mark

type Board a =
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

emptyBoard :: Board Space
emptyBoard =
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
