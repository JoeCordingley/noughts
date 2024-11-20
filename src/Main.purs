module Main where

import Prelude

-- import Data.Array (replicate)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Tuple (Tuple)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
-- import Deku.Hooks (useState, (<#~>))
import Deku.Hooks (useState, (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Poll (Poll)
import Data.Lens (Lens', view, _1, over, set, lens)
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

lenses :: Array (Lens' Board Space)
lenses = [ _nw, _n, _ne, _w, _c, _e, _sw, _s, _se ]

main :: Effect Unit
main = do
  void $ runInBody Deku.do
    setBoard /\ board <- useState startingGame
    D.div [ DA.klass_ "p-6 bg-white rounded-lg shadow-lg" ]
      [ D.h1 [ DA.klass_ "text-2xl font-bold text-center mb-4" ] [ text_ "Noughts and Crosses" ]
      , D.div [ DA.klass_ "grid grid-cols-3 gap-2 w-64 mx-auto" ] (lenses <#> (\l -> spaceDiv setBoard board (combineLenses (_board <<< l) _turn)))
      ]

combineLenses :: forall s a b. Lens' s a -> Lens' s b -> Lens' s (Tuple a b)
combineLenses lensA lensB =
  lens getter (flip setter)
  where
  getter s = (view lensA s) /\ (view lensB s)
  setter (a /\ b) = set lensB b <<< set lensA a

type Hook a = Tuple (a -> Effect Unit) (Poll a)

spaceDiv :: forall s. (s -> Effect Unit) -> Poll s -> Lens' s (Tuple Space Mark) -> Nut
spaceDiv setS poll lens =
  ( D.div
      [ DA.klass_ "cell flex items-center justify-center w-20 h-20 bg-gray-200 text-3xl font-bold rounded cursor-pointer hover:bg-gray-300"
      , DL.runOn DL.click $ poll <#> (setS <<< f)
      ]
      [ poll <#> view (_1 >>> lens) <#~> case _ of
          Just Cross -> text_ "X"
          Just Nought -> text_ "O"
          Nothing -> text_ ""
      ]
  )
  where
  f s = over lens g s
    where
    g (Nothing /\ Nought) = Just Nought /\ Cross
    g (Nothing /\ Cross) = Just Cross /\ Nought
    g (Just mark /\ turn) = Just mark /\ turn

_turn :: Lens' Game Mark
_turn = prop (Proxy :: Proxy "turn")

_nw :: Lens' Board Space
_nw = prop (Proxy :: Proxy "nw")

_n :: Lens' Board Space
_n = prop (Proxy :: Proxy "n")

_ne :: Lens' Board Space
_ne = prop (Proxy :: Proxy "ne")

_w :: Lens' Board Space
_w = prop (Proxy :: Proxy "w")

_c :: Lens' Board Space
_c = prop (Proxy :: Proxy "c")

_e :: Lens' Board Space
_e = prop (Proxy :: Proxy "e")

_sw :: Lens' Board Space
_sw = prop (Proxy :: Proxy "sw")

_s :: Lens' Board Space
_s = prop (Proxy :: Proxy "s")

_se :: Lens' Board Space
_se = prop (Proxy :: Proxy "se")

_board :: Lens' Game Board
_board = prop (Proxy :: Proxy "board")

data Mark = Nought | Cross
type Space = Maybe Mark
type Game = { turn :: Mark, board :: Board }

startingGame :: Game
startingGame = { turn: Nought, board: emptyBoard }

type Board =
  { nw :: Space
  , n :: Space
  , ne :: Space
  , w :: Space
  , c :: Space
  , e :: Space
  , sw :: Space
  , s :: Space
  , se :: Space
  }

emptyBoard :: Board
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
