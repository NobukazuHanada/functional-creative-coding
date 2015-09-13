module Main where

import Prelude
import Data.Array
import Data.Array.ST
import Data.Int
import Data.Foldable
import Data.Maybe
import Control.Monad.Eff
import Control.Monad.ST 
import Control.Apply
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console
import Math (sin,cos,pow,pi)
import Sketch
import Noise
import Graphics.Canvas.Free (Graphics(), rect)
import Graphics.Canvas (Canvas())
import DOM
import Signal ((<~), runSignal)
import Signal.DOM
import Signal.Time


main = do
  width <- windowWidth
  height <- windowHeight

  -- runSketch simpleLine
  runSketch draw1
  -- runSketch manyFracDraw
  -- runSketch sampleLine
  -- runSketch $ randomLine width height
  -- runSketch $ perlinLine width height 0.0
  -- runSketch $ myComplexLine width height
  -- runSketch $ circle 200.0
  -- runSketch $ spiral
  -- runSketch $ noizySpiral
  runSketch $ manyNoizySpiral 100
  -- runSketch (myComplexCircle 200.0)
  
  -- runSignal $ map timeIsMoney (every second)
  -- runSignal $ map (runSketch <<< perlinLine width height <<< (* 0.0005)) (every millisecond)
  -- runSignal $ map (runSketch <<< perlin2dLine width height <<< (* 0.0005)) (every millisecond)
  -- moveRect
  


moveRect = do
  pos <- mousePos
  runSketchSignal $ map (\pos -> rect { x : (toNumber pos.x),
                                        y : (toNumber pos.y),
                                        w : 100.0,
                                        h : 100.0}) pos 

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
    *> myTo {x:x1, y:0.0}
    *> myTo {x:x2, y:0.0}
    *> myTo {x:x3, y:0.0}
    where
      myTo pos = to {x:pos.x - 20.0, y:0.0} *> origin {x:pos.x, y:0.0}
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


--- 汎用的な
genericLines :: Number -> Number -> Array Point -> Graphics Unit
genericLines width height posList =
  case uncons resizePos of
    Nothing ->
      origin {x:0.0, y:0.0}
    Just unconsed ->
      foldl (*>) (origin unconsed.head) (map to unconsed.tail)
  where
    resizePos = map (\pos -> {x : pos.x * ( width/2.0),
                              y : pos.y * (-height/2.0)}) posList


createPosList :: Array Number -> Array Point
createPosList yList =
  zipWith (\x y -> {x:x, y:y}) xs yList
  where
    n = length yList
    xs = map (toNumber >>> (/ (toNumber  n)) >>> ((-) 0.5) >>> (* 2.0)) (0..n)

--- genericLinesのテスト
sampleLine = do
  width <- windowWidth
  height <- windowHeight
  return $ genericLines width height posList
  where
    posList = [{x:(-1.0), y:(-1.0)},
               {x:1.0, y:1.0}]

-- ランダムな線
randomLine width height = do
  values <- map createPosList (randomYLists 100)
  return $ genericLines width height values


randomYLists n = 
  replicateM n $ map ((-) 0.5) random
  
  
-- パーリンノイズ
perlinLine width height offset = do
  ys <- perlinList 200 offset
  return (genericLines width height $ createPosList ys)

perlinList n offset = 
 foldl (lift2 $ flip (:)) (pure []) noises
 where
   noises = map (toNumber >>> (/ 60.0) >>> (+ offset) >>> noise1d) (1..n)



-- 2d 
perlin2dLine width height offset = do
  ys <- perlin2dList 100 offset
  return (genericLines width height $ createPosList ys)

perlin2dList n offset = 
 foldl (lift2 $ flip (:)) (pure []) noises
 where
   noises = map (toNumber
                 >>> (/ (toNumber n))
                 >>> (* 0.8)
                 >>> noise2d offset
                 >>> (map $ ((-) 0.5) >>> (* 2.0)))
            (1..n)


--- 三角関数
sinLine :: Number -> Number -> forall eff. Eff eff (Graphics Unit)
sinLine width height = 
  return $ genericLines width height (createPosList (map (toNumber >>> (/ 5.0) >>> sin >>> (* 0.4)) (0..100)))
  
--- ふくざつな数式
myComplexLine :: Number -> Number -> forall eff. Eff eff (Graphics Unit)
myComplexLine width height = 
  return $ genericLines width height (createPosList (map (toNumber >>> (/ 100.0) >>> myComplexFunc) (0..1000)))

myComplexFunc = lift3 (\x y z -> x + y + z)
                      (sin >>> (* 0.8) >>> (flip pow 5.0))
                      ((* 4.0) >>> sin >>> (* 0.1)) 
                      ((* 20.0) >>> sin >>> (* 0.02))


-- 円を分解して足す
circle :: Number -> forall e. Eff e (Graphics Unit)
circle r = 
  return (foldl (*>) (origin {x:x0, y:y0}) manyTo)
  where
    x0 = r * (cos 0.0)
    y0 = r * (sin 0.0)
    angles = map (toNumber >>> (/ 360.0) >>>  (* pi) >>> (* 2.0)) (1..360)
    xs = map (cos >>> (* r)) angles
    ys = map (sin >>> (* r)) angles
    posList = zipWith (\x y -> {x:x, y:y}) xs ys
    manyTo = map to posList


-- らせん
spiral :: forall e. Eff e (Graphics Unit)
spiral = do
  return (foldl (*>) (origin {x:0.0, y:0.0}) manyTo)
  where
    rs = map (toNumber >>> (* 0.1)) (0..5000)
    angles = map (toNumber >>> (/ 360.0) >>>  (* pi) >>> (* 2.0)) (1..5000)
    xs = zipWith (*) rs $ map cos angles
    ys = zipWith (*) rs $ map sin angles 
    posList = zipWith (\x y -> {x:x, y:y}) xs ys
    manyTo = map to posList


--- のいずらせん
noizySpiral = do
  noiseList <- replicateM 700 random
  return (foldl (*>) (origin {x:0.0, y:0.0}) $ manyTo (rs (map (((-) 0.5) >>> (* 200.0)) noiseList)))
  where 
    angles = map (toNumber >>> (/ 360.0) >>>  (* pi) >>> (* 2.0)) (1..2000)
    xs = zipWith (*) (map cos angles)
    ys = zipWith (*) (map sin angles) 
    posList = lift2 (zipWith (\x y -> {x:x, y:y})) xs ys
    manyTo = (map to) <<< posList
    rs = zipWith (+) (map (toNumber >>> (* 0.5)) (0..5000))



--- manyノイズ    
manyNoizySpiral n =
  foldl (lift2 (*>)) (return $ color "#ccc") $ map (\startAngle -> aNoizySpiral (map (toNumber >>> (/ 30.0) >>> (* (2.0 * pi)))
                                                                        (startAngle..100)))
                                          (0..n)


aNoizySpiral angles = do
  noiseList <- replicateM 600 random
  return (foldl (*>) (color "#ccc" *> origin {x:0.0, y:0.0}) $ manyTo (rs (map (((-) 0.5) >>> (* 200.0)) noiseList)))
  where 
    xs = zipWith (*) (map cos angles)
    ys = zipWith (*) (map sin angles) 
    posList = lift2 (zipWith (\x y -> {x:x, y:y})) xs ys
    manyTo = (map to) <<< posList
    rs = zipWith (+) (map (toNumber >>> (* 10.0)) (0..5000))


--- 円 ++ myComplexFunc
myComplexCircle :: Number -> forall e. Eff e (Graphics Unit)
myComplexCircle size = do
  return (foldl (*>) (origin {x:x0, y:y0}) manyTo)
  where
    r0 = (myComplexFunc >>> (* 100.0) >>> (+ size)) 0.0
    x0 = r0 * (cos 0.0)
    y0 = r0 * (sin 0.0)
    
    rs = map (toNumber >>> (/ 360.0) >>> (* (pi * 2.0)) >>> myComplexFunc >>> (* 100.0) >>> (+ size) ) (0..2880)
    angles = map (toNumber >>> (/ 2880.0) >>>  (* pi) >>> (* 2.0)) (1..2880)
    xs = zipWith (*) rs $ map cos angles
    ys = zipWith (*) rs $ map sin angles 
    posList = zipWith (\x y -> {x:x, y:y}) xs ys
    manyTo = map to posList

timeIsMoney :: Number -> forall e. Eff (canvas :: Canvas, dom :: DOM | e) Unit
timeIsMoney r =
  runSketch (return $ origin {x:0.0, y:0.0} *> to {x: 200.0 * (cos (r / 60000.0 * pi)),
            y: 200.0 * (sin (r / 60000.0 * pi))})



                                       
