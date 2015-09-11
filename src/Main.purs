module Main where

import Prelude
import Data.Array
import Data.Int
import Data.Foldable
import Control.Monad.Eff
import Control.Apply
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console
import Sketch
import Noise
import Graphics.Canvas.Free (Graphics())


main = do
  x <- perlin2 1.0 1.0
  log x
  runSketch manyfracDraw


simpleLine = do 
  width <- windowWidth
  height <- windowHeight
  
  return $ do 
         color "#000000"
         origin {x : (- width / 2.0), y : 0.0 }
         to {x : (width / 2.0), y : 0.0 }

-- ちょっと3分割
draw1 = do
  width <- windowWidth
  height <- windowHeight
  return $ line1 width height

line1 :: Number -> Number -> Graphics Unit
line1 width height =
  origin {x:x0, y:0.0} 
    *> to {x:x1, y:0.0}
    *> to {x:x2, y:0.0}
    *> to {x:x3, y:0.0}
    where
      a = width / 3.0
      x0 = - (width / 2.0)
      x1 = a + x0
      x2 = a * 2.0 + x0
      x3 = width


-- たくさん分割
manyfracDraw = do
  width <- windowWidth
  height <- windowHeight
  return $ manyfracLine width height

manyfracLine width height =
  foldl (*>) (origin {x:x0, y:0.0}) manyTo
  where
    n = 100
    x0 = - (width / 2.0)
    xs = map (toNumber >>> (/ (toNumber n)) >>> (* width) >>> (+ x0)) (0..n)
    manyPos = map (\x -> {x:x, y:0.0}) xs
    manyTo = map to manyPos

-- パーリンノイズ
manyfracDraw = do
  
