module Main where

import Prelude

import Data.Lens (Lens', traverseOf, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Poll (Poll)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- import Unsafe.Coerce (unsafeCoerce)

lenses :: Array BoardLens
lenses = [ _nw, _n, _ne, _w, _c, _e, _sw, _s, _se ]

main :: Effect Unit
main = do
  void $ runInBody Deku.do
    setGame /\ game <- useState startingGame
    D.div [ DA.klass_ "p-6 bg-white rounded-lg shadow-lg" ]
      [ D.h1 [ DA.klass_ "text-2xl font-bold text-center mb-4" ] [ text_ "Noughts and Crosses" ]
      , D.div [ DA.klass_ "grid grid-cols-3 gap-2 w-64 mx-auto" ] (lenses <#>(\l -> squareDiv (setSquare l setGame) (pollSquare l game)))
      ]

eventToPoll :: forall a. Event a -> Poll a
eventToPoll = flap


setSquare :: BoardLens -> (Game -> Effect Unit) -> Effect Unit
setSquare = unsafeCoerce unit

pollSquare :: BoardLens -> Poll Game -> Poll Square
pollSquare = unsafeCoerce unit

type BoardLens = forall a. Lens' (Board a) a 

winningSquare :: BoardLens -> Status -> Boolean
winningSquare l = case _ of 
  Finished board -> (getDisj <<< view l) board
  _ -> false

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

updateBoard :: BoardLens -> Game -> Maybe Game
updateBoard lens {board, status} = case status of
  Playing turn -> traverseOf lens playTurn board <#> nextTurn where
    playTurn Nothing = Just (Just turn)
    playTurn _ = Nothing
    nextTurn newBoard = {status: newStatus, board: newBoard} where
      newStatus = case wonBoard board of
        Just won -> Finished won
        Nothing -> Playing $ case turn of
          Nought -> Cross
          Cross -> Nought
  _ -> Nothing

wonBoard :: Board Space -> Maybe (Board (Disj Boolean))
wonBoard _ = Nothing


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
