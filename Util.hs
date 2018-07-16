{-# LANGUAGE FlexibleContexts, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, ViewPatterns #-}
module Util where

import Apecs
import Apecs.Util
import Data
import Linear
import Data.Foldable
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle        (radToDeg)

boxy (V2 c1@(realToFrac -> c1' :: Double) c2@(realToFrac -> c2'))
      (V2 w@(realToFrac -> w' :: Double) h@(realToFrac -> h'))
  = newEntity (StaticBody, Wall, Angle 0, Position (V2 c1' c2'), Box (V2 c1' c2', w', h'), BodyPicture $ color red $ rectangleSolid (2*w) (2*h) )
           

aabb (Box (V2 ax ay,w1,h1)) (Box (V2 bx by,w2,h2)) -- edge collision detection between boxes centered at (ax,ay) and (bx,by), with half-widths and half-heights w1,w2,h1,h2
  | ax - w1 <= bx + w2
    && ax + w1 >= bx - w2
    && ay - h1 <= by + h2 
    && ay + h1 >= by - h2 = True
  | True                  = False

box (V2 x y) w h = Box ((V2 x y), w, h)

handlePic :: Maybe Picture -> IO Picture
handlePic = maybe
  (error "img not loading")
  return
  
cycle1 (h:hs) = hs ++ [h]

vToRad (V2 m n) = case compare m 0 of
  LT -> atan ( n / m )  + pi
  GT -> atan ( n / m ) 
  EQ -> (-pi)/2 * (signum n)

applyView :: Camera -> Picture -> Picture
applyView (Camera (V2 x y) scale) =
  Scale (realToFrac scale) (realToFrac scale) .  Translate (realToFrac . negate $ x) (realToFrac . negate $ y)

mouseToWorld :: (Float,Float) -> Camera -> V2 Double
mouseToWorld (x,y) (Camera offset scale) = (/scale) <$> (V2 (realToFrac x) (realToFrac y))-offset

v2ToTuple :: V2 Double -> (Float, Float)
v2ToTuple (V2 x y) = (realToFrac x, realToFrac y)

data Convex = Convex { vertices :: [V2 Double], radius :: Double } deriving (Eq, Show)

toPicture :: Convex -> Picture
toPicture (Convex [V2 x y] radius) = Translate (realToFrac x) (realToFrac y) $ Circle (realToFrac radius)
toPicture (Convex [a,b] _) = Line [v2ToTuple a, v2ToTuple b]
toPicture (Convex verts _) = Polygon (v2ToTuple <$> verts)

-- | Translates all vertices. The name shift is to prevent collisions with gloss
shift :: V2 Double -> Convex -> Convex
shift = mapVertices . (+)

--get the camera to follow the player
updateCamera :: Has World Camera => System World ()
updateCamera = do
  cmapM_ $ \(Player, Position p) -> modify global $ \(Camera o s) -> (Camera p s)
       
-- | Map a function over all vertices
mapVertices :: (V2 Double -> V2 Double) -> Convex -> Convex
mapVertices f (Convex s r) = Convex (f <$> s) r

hLine, vLine :: Double -> Convex
hLine l = Convex [V2 (-l/2) 0, V2 (l/2) 0] 0
vLine l = Convex [V2 0 (l/2), V2 0 (-l/2)] 0
