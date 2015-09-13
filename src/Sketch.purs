module Sketch where

import Prelude
import Data.Maybe
import Data.Foldable
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Graphics.Canvas (getCanvasElementById, getContext2D,setCanvasWidth,setCanvasHeight)
import Graphics.Canvas.Free
import Signal
import DOM

type Point = { x :: Number, y :: Number }

foreign import windowWidth :: forall eff. Eff ( dom :: DOM | eff ) Number
foreign import windowHeight :: forall eff. Eff ( dom :: DOM | eff ) Number




origin :: Point -> Graphics Unit
origin pos = moveTo pos.x pos.y

to :: Point -> Graphics Unit
to pos = lineTo pos.x pos.y

color :: String -> Graphics Unit
color = setStrokeStyle

runSketch sketch = do
    Just canvas <- getCanvasElementById "canvas"
    width <- windowWidth
    height <- windowHeight
    setCanvasWidth width canvas
    setCanvasHeight height canvas
    context <- getContext2D canvas
    graphics <- sketch
    
    runGraphics context $ do
      translate (width / 2.0) (height / 2.0)
      beginPath
      graphics
      stroke


runSketchSignal sketchSignal = do
  Just canvas <- getCanvasElementById "canvas"
  width <- windowWidth
  height <- windowHeight
  setCanvasWidth width canvas
  setCanvasHeight height canvas
  context <- getContext2D canvas
    
  runSignal $ map (\graphics -> do
                    runGraphics context $ do
                      beginPath
                      graphics
                      stroke) sketchSignal

