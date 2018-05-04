module Util where

import Apecs
import Apecs.Util
import Apecs.Physics
import Data
import Graphics.Gloss

aabb (V2 ax ay,w1,h1) (V2 bx by,w2,h2) -- box collision detection centered at (ax,ay) and (bx,by), with half-widths and half-heights w1,w2,h1,h2
  | ax - w1 <= bx + w2
    && ax + w1 >= bx - w2
    && ay - h1 <= by + h2 
    && ay + h1 >= by - h2 = True
  | True                  = False

a_material = (Friction 0.2, Elasticity 0.4, Density 1)

box (V2 x y) w h = Box ((V2 x y), w, h)

handlePic :: Maybe Picture -> IO Picture
handlePic = maybe
  (error "img not loading")
  return
  
cyclePic (h:hs) = hs ++ [h]

vToRad (V2 m n) = case compare m 0 of
  LT -> atan ( n / m )  + pi
  GT -> atan ( n / m ) 
  EQ -> (-pi)/2 * (signum n)

