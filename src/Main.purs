module Main where

import Graphics.Canvas
import Prelude

import Control.Monad.Eff (Eff)
import Data.Date (year)
import Data.Traversable
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Point (drawPoint)

recursion :: Number -> Number -> String -> Array Record
recursion x y z =if y/=500.0 
  then (if x/=500.0 
    then [{x: x, y:y, color:z}] <> recursion (x+10.0) y z 
    else [] <> recursion 100.0 (y + 10.0) z) 
  else []

type Record = {
  x :: Number,
  y :: Number,
  color :: String

}

matrix :: Array Record
matrix = recursion 100.0 100.0 "#ff0000"


main :: forall e. Eff (canvas :: CANVAS | e) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  
  traverse (drawPoint ctx) matrix
  -- pure unit 