module Main where

import Prelude

import Deku.Control (text_)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Array (replicate)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.Do as Deku
import Deku.Hooks (useState)
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut)
import Data.Identity (Identity(..))
import Data.Const (Const(..))
import Data.Newtype (unwrap)

main :: Effect Unit
main = do
  void $ runInBody Deku.do
    _ /\ _ <- useState emptyBoard
    D.div [ DA.klass_ "p-6 bg-white rounded-lg shadow-lg" ]
      [ D.h1 [ DA.klass_ "text-2xl font-bold text-center mb-4" ] [ text_ "Noughts and Crosses" ]
      , D.div [ DA.klass_ "grid grid-cols-3 gap-2 w-64 mx-auto" ] (replicate 9 space)
      ]

space :: Nut
space = (D.div [ DA.klass_ "cell flex items-center justify-center w-20 h-20 bg-gray-200 text-3xl font-bold rounded cursor-pointer hover:bg-gray-300" ] [])

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

_nw :: Lens Board Space
_nw f board = (\nw -> board { nw = nw }) <$> f board.nw

_n :: Lens Board Space
_n f board = (\n -> board { n = n }) <$> f board.n

_ne :: Lens Board Space
_ne f board = (\ne -> board { ne = ne }) <$> f board.ne

_w :: Lens Board Space
_w f board = (\w -> board { w = w }) <$> f board.w

_c :: Lens Board Space
_c f board = (\c -> board { c = c }) <$> f board.c

_e :: Lens Board Space
_e f board = (\e -> board { e = e }) <$> f board.e

_sw :: Lens Board Space
_sw f board = (\sw -> board { sw = sw }) <$> f board.sw

_s :: Lens Board Space
_s f board = (\s -> board { s = s }) <$> f board.s

_se :: Lens Board Space
_se f board = (\se -> board { se = se }) <$> f board.se

over :: forall a s. (a -> a) -> Lens s a -> s -> s
over f l = unwrap <<< (l $ Identity <<< f)

view :: forall a s. Lens s a -> s -> a
view l = unwrap <<< (l Const)

data Mark = Nought | Cross
type Space = Maybe Mark
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
