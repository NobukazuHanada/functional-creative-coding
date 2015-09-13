module Noise where

import Prelude
import Control.Monad.Eff

foreign import data Noise :: !

foreign import noise1d :: forall eff. Number -> Eff ( noise :: Noise | eff ) Number 
foreign import noise2d :: forall eff. Number -> Number  -> Eff ( noise :: Noise | eff ) Number 
foreign import noise3d :: forall eff. Number -> Number -> Number -> Eff ( noise :: Noise | eff ) Number 


foreign import seed :: forall eff. Number -> Eff ( noise :: Noise | eff ) Unit 
        
