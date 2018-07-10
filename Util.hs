{-# LANGUAGE FlexibleContexts, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module Util where

import Apecs
import Apecs.Util
import Data
import Linear
import Data.Foldable
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle        (radToDeg)

abBox b1@(Box (V2 ax ay,w1,h1)) b2@(Box (V2 bx by,w2,h2)) =
  let w = [ax - w1 >= bx + w2
          , ax + w1 <= bx - w2
          , ay - h1 >= by + h2 
          , ay + h1 <= by - h2
          ]
  in helper
  where helper 
          | ax - w1 >= bx + w2 = box (V2 (bx + w2) ay) w1 h1
          | ax + w1 <= bx - w2 = box (V2 (bx - w2) ay) w1 h1
          | ay - h1 >= by + h2 = box (V2 ax (by + h2)) w1 h1
          | ay + h1 <= by - h2 = box (V2 ax (by - h2)) w1 h1
          | True               = b1
    
--}
boxInBox (Box (V2 ax ay,w1,h1)) (Box (V2 bx by,w2,h2))
  | ea1 > ea2 && ew1 < ew2 && es1 > es2 && ed1 < ed2 = True
  | True                                             = False
    -- | ea1 < ea2 && ew1 > ew2 && es1 < es2 && ed1 > ed2
  where
   ea1 = ax - w1
   ea2 = bx - w2
   ew1 = ay + h1
   ew2 = by + h2
   es1 = ay - h1
   es2 = by - h2
   ed1 = ax + w1
   ed2 = bx + w2

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
{--
drawWorld :: (Has w Position, Has w Angle, Has w BodyPicture, Has w Camera) => System w Picture
drawWorld = do
  f <- cmapM $ \((Position (V2 x y), Angle theta, BodyPicture pic)) ->
        return . Translate (realToFrac x) (realToFrac y) . Rotate (negate . radToDeg . realToFrac $ theta) $ pic

  [view] <- cmapM $ \(c@(Camera _ _)) -> return c 
  return . applyView view . fold $ f
--}

{--
drawSystem :: System World Picture
drawSystem = do
  --f <- flip fmap (get global) $ \((Position (V2 x y), Angle theta, BodyPicture pic)) -> Translate (realToFrac x) (realToFrac y) . Rotate (negate . radToDeg . realToFrac $ theta) $ pic
  cmapM $ \((Position (V2 x y), Angle theta, BodyPicture pic)) -> do
    view <- get global :: System World Camera
    f <- return . Translate (realToFrac x) (realToFrac y) . Rotate (negate . radToDeg . realToFrac $ theta) $ pic
    return . applyView view . mconcat $ f 
--}

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
