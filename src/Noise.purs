module Noise where

import Prelude
import Control.Monad.Eff

foreign import data Noise :: !

foreign import simplex2 :: forall eff. Number -> Number -> Eff ( noise :: Noise | eff ) Number 
foreign import simplex3 :: forall eff. Number -> Number -> Number -> Eff ( noise :: Noise | eff ) Number 


foreign import perlin2 :: forall eff. Number -> Number -> Eff ( noise :: Noise | eff ) Number 
foreign import perlin3 :: forall eff. Number -> Number -> Number -> Eff ( noise :: Noise | eff ) Number 

foreign import seed :: forall eff. Number -> Number -> Eff ( noise :: Noise | eff ) Unit
