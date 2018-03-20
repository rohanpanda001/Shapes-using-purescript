module Point where
  
import Graphics.Canvas
import Prelude

type Rectangle =
  { x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  }
  
rect :: Number -> Number -> Number -> Number -> Rectangle
rect nx ny nw nh= {x: nx, y: ny, w: nw , h: nh}


drawPoint ctx a = void $ do
  _ <- setFillStyle a.color ctx
  _ <- setStrokeStyle a.color ctx
  
  let rec = rect a.x a.y 2.0 2.0

  _ <- fillRect ctx rec
  _ <- strokeRect ctx rec
  pure unit