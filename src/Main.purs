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
import Data.Lens (Lens', set, view)
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

lenses :: Array (Lens' Board Space)
lenses = [ _nw, _n ]

-- lenses = [ _nw, _n, _ne, _w, _c, _e, _sw, _s, _se ]

main :: Effect Unit
main = do
  void $ runInBody Deku.do
    hook <- useState emptyBoard
    D.div [ DA.klass_ "p-6 bg-white rounded-lg shadow-lg" ]
      [ D.h1 [ DA.klass_ "text-2xl font-bold text-center mb-4" ] [ text_ "Noughts and Crosses" ]
      , D.div [ DA.klass_ "grid grid-cols-3 gap-2 w-64 mx-auto" ] (lenses <#> lensHook hook >>> spaceDiv)
      ]

type Hook a = Tuple (a -> Effect Unit) (Poll a)

lensHook :: forall s a. Hook s -> Lens' s a -> Hook a
lensHook (_ /\ pollS) l = setA /\ pollA
  where
  setA = unsafeCoerce unit
  pollA = pollS <#> view l

--spaceNut :: (Space -> Effect Unit) -> Poll Space -> Nut
--spaceNut = unsafeCoerce unit

spaceDiv :: Hook Space -> Nut
spaceDiv (setSpace /\ space) =
  ( D.div [ DA.klass_ "cell flex items-center justify-center w-20 h-20 bg-gray-200 text-3xl font-bold rounded cursor-pointer hover:bg-gray-300", DL.click_ \_ -> setSpace (Just Cross) ]
      [ space <#~> case _ of
          Just Cross -> text_ "X"
          Just Nought -> text_ "O"
          Nothing -> text_ ""
      ]
  )

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
