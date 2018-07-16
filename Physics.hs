{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Physics where

import Apecs
import Apecs.Util
import Control.Concurrent
import Graphics.Gloss
import Linear
import Data
import Util

stepPhysics :: Double -> System World ()
stepPhysics dT = do
  --moves all objects that are in motion (have a Velocity)
  Gravity g <- get global
  cmap $ \(Position p, Velocity v) -> (Position $ p + v, Velocity $ v + g)
  
  --correct angle for arrows
  cmap $ \(Velocity v, Angle t, Projectile) -> (Projectile, Angle $ vToRad v + pi / 2)
        
  --updates all hit boxes to current position
  cmap $ \(Box (b, w, h), Position p) -> (Box (p, w, h))
  

playerBoxBound :: System World ()
playerBoxBound = do
  cmapM_ $ \( b@(Box (c, w, h)), _ :: Not Wall, e) -> cmapM_ $ wallBounceE e b
  where
    wallBounceE e b@(Box (c, w, h)) (Wall, wb@(Box (c2, w2, h2))) =
      if aabb b wb --bounds checking for Projectiles
      then do
        
        (Position p@(V2 p1 p2), Velocity v@(V2 v1 v2)) <- get e

        --checks all 4 ortho directions based on current velocity to determine the bounce behavior
        let boxChecks = (\ z1 z2 -> aabb (box (c + V2 (z1*v1) (z2*v2)) w h) wb) <$> [1,-1] <*> [-1,1]

        case boxChecks of
          
          [False,_,_,_] -> e `set` (Position $ p + V2 v1 (negate v2), Velocity (V2 v1 (negate v2) / 5))
          [_,False,_,_] -> e `set` (Position $ p + v, Velocity (v / 5))
          [_,_,False,True] -> e `set` (Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) (negate v2) / 5))
          [_,_,_,False] -> e `set` (Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) v2 / 5))
          otherwise  -> e `set` (Position $ p + negate v, Velocity (negate v / 5))
         
      else return ()
