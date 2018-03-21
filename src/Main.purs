module Main where

import Graphics.Canvas
import Prelude

import Control.Monad.Eff (Eff)
import Data.Date (year)
import Data.Traversable
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Point (drawPoint)

type Record = {
  x :: Number,
  y :: Number,
  color :: String

}

-- Rectangle Logic

rectLines :: Number -> Number -> String -> Array Record
rectLines x y z = if y/=500.0 
  then (if x/=500.0 
    then [{x: x, y:y, color:z}] <> rectLines (x+10.0) y z 
    else [] <> rectLines 100.0 (y + 10.0) z) 
  else []

rectMatrix :: Array Record
rectMatrix = rectLines 100.0 100.0 "#ff0000"

drawFilledRectangle ctx = traverse (drawPoint ctx) rectMatrix

-- Triangle Logic

triLines :: Number -> Number -> String -> String -> Array Record
triLines x y z dir = if y/=800.0 && dir == "down"
  then ([{x: x, y:y, color:z}] <> triLines (x-10.0) (y+10.0) z dir)
  else (if (x/=500.0 && dir == "down")
    then ([{x: x, y:y, color:z}] <> triLines (x+10.0) y z dir)
    else if y/=600.0
      then [{x: x, y:y, color:z}] <> triLines (x-10.0) (y-10.0) z "up"
    else [])

triMatrix :: Array Record
triMatrix =  triLines 300.0 600.0 "#ff0000" "down"

drawTriangle ctx = do
  traverse (drawPoint ctx) triMatrix

-- Diamond Logic

diaLines :: Number -> Number -> String -> String -> Array Record
diaLines x y z dir = if y<1200.0 && dir == "down"
  then ([{x: x, y:y, color:z}] <> diaLines (x-10.0) (y+10.0) z dir)
  else if (x<300.0 && dir == "down")
    then ([{x: x, y:y, color:z}] <> diaLines (x+10.0) (y+10.0) z dir)
    else if x<500.0 && dir /= "up-right"
      then [{x: x, y:y, color:z}] <> diaLines (x+10.0) (y-10.0) z "up-down"
      else if y/=1000.0 
        then [{x: x, y:y, color:z}] <> diaLines (x-10.0) (y-10.0) z "up-right"
      else []

diaMatrix :: Array Record
diaMatrix =  diaLines 300.0 1000.0 "#ff0000" "down"

drawDiamond ctx = do
  traverse (drawPoint ctx) diaMatrix


main :: forall e. Eff (canvas :: CANVAS | e) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  -- Draw Rectangle
  _ <- drawFilledRectangle ctx

  -- Draw Triangle
  _ <- drawTriangle ctx

    -- Draw Diamond
  _ <- drawDiamond ctx

  pure unit 