{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Ceph.Physics.Box where

import Ceph.Components

import Graphics.Gloss
import Apecs
import Linear

wall -- (V2 c1@(realToFrac -> c1' :: Double) c2@(realToFrac -> c2'))
     (V2 c1' c2')
     (V2 w@(realToFrac -> w' :: Double) h@(realToFrac -> h'))
  = newEntity (StaticBody, Wall, Angle 0, Position (V2 c1' c2'), Box (V2 c1' c2', w', h'), BodyPicture $ color red $ rectangleSolid (2*w) (2*h) )
           

aabb (Box (V2 ax ay,w1,h1)) (Box (V2 bx by,w2,h2)) -- edge collision detection between boxes centered at (ax,ay) and (bx,by), with half-widths and half-heights w1,w2,h1,h2
  | ax - w1 <= bx + w2
    && ax + w1 >= bx - w2
    && ay - h1 <= by + h2 
    && ay + h1 >= by - h2 = True
  | True                  = False

box (V2 x y) w h = Box ((V2 x y), w, h)

boxBound :: System World ()
boxBound = do
  cfoldM ( \_ ( b@(Box (c, w, h)), _ :: Not Wall, e) -> cmapM_ $ wallBounceE e b) ()
  --bs <- getAll :: System World [(Box, Entity, Not Wall)]
  --concurrently ((\(b,e,_) -> cmapM_ (wallBounceE e b)) <$> bs) 
  where
    wallBounceE e b@(Box (c, w, h)) (Wall, wb@(Box (c2, w2, h2))) =
      if aabb b wb
      then do
        
        (Position p@(V2 p1 p2), Velocity v@(V2 v1 v2)) <- get e

        --checks all 4 orthogonal directions to current velocity to determine the bounce behavior
        let boxChecks = (\ z1 z2 -> aabb (box (c + V2 (z1*v1) (z2*v2)) w h) wb) <$> [1,-1] <*> [-1,1] -- = [[1,-1],[1,1],[-1,1],[-1,-1]]
        let friction = 4
        case boxChecks of
          
          [False,_,_,_] -> e `set` (Position $ p + V2 v1 (negate v2), Velocity (V2 v1 (negate v2) / friction))
          [_,False,_,_] -> e `set` (Position $ p + v, Velocity (v / friction))
          [_,_,False,True] -> e `set` (Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) (negate v2) / friction))
          [_,_,_,False] -> e `set` (Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) v2 / friction))
          otherwise  -> e `set` (Position $ p + negate v, Velocity (negate v ))
         
      else return ()
