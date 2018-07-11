{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
  Gravity g <- get global :: System World Gravity 
  cmap $ \(Position p, Velocity v) -> (Position $ p + v, Velocity $ v + g)
  --correct angle for arrows
  cmap $ \(Velocity v, Angle t, Projectile) -> (Projectile, Angle $ vToRad v + pi /2)
        
  --updates all hit boxes to current position
  cmap $ \(Box (b, w, h), Position p) -> (Box (p, w, h))
  --boxCheck --cmapM_ $ \(Player, Velocity v) -> liftIO $ print v


playerBoxBound :: System World ()
playerBoxBound = do
  cmapM_ $ \(Player, b@(Box (c, w, h))) -> cmapM_ $ wallBouncePlayer b
  cmapM_ $ \(Projectile, b@(Box (c, w, h)), e) -> cmapM_ $ wallBounceProjE e Projectile b
  where
    wallBounceProjE e o b@(Box (c, w, h)) (Wall, wb@(Box (c2, w2, h2))) =
      if aabb b wb
      then do
        (Projectile, Position p@(V2 p1 p2), Velocity v@(V2 v1 v2)) <- get e
        case (\z1 z2 -> aabb (box (c + V2 (z1*v1) (z2*v2)) w h) wb) <$> [1,-1] <*> [-1,1] of
          [False,_,_,_] -> e `set` (o, Position $ p + V2 v1 (negate v2), Velocity (V2 v1 (negate v2) / 5))
          [_,False,_,_] -> e `set` (o, Position $ p + v, Velocity (v / 5))
          [_,_,False,_] -> e `set` (o, Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) (negate v2) / 5))
          [_,_,_,False] -> e `set` (o, Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) v2 / 5))
         
          -- [True,True,True,True] -> (o, Position $ p + negate v, Velocity $  negate (v/5))
          otherwise  -> e `set` (o, Position $ p + negate v, Velocity (negate v / 5))
         
      else return () 
  
    wallBounceProj b@(Box (c, w, h)) (Wall, wb@(Box (c2, w2, h2))) =
      if aabb b wb
      then cmap $ \(Projectile, Position p@(V2 p1 p2), Velocity v@(V2 v1 v2)) ->  case (\z1 z2 -> aabb (box (c + V2 (z1*v1) (z2*v2)) w h) wb) <$> [1,-1] <*> [-1,1] of
         [False,_,_,_] -> (Projectile, Position $ p + V2 v1 (negate v2), Velocity (V2 v1 (negate v2) / 5))
         [_,False,_,_] -> (Projectile, Position $ p + v, Velocity (v / 5))
         [_,_,False,_] -> (Projectile, Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) (negate v2) / 5))
         [_,_,_,False] -> (Projectile, Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) v2 / 5))
         
         [True,True,True,True] -> (Projectile, Position $ p + negate v, Velocity $  negate (v/5))
         otherwise  -> (Projectile, Position $ p + negate v, Velocity (negate v / 5))
         
      else return () 
  
    wallBouncePlayer b@(Box (c, w, h)) (Wall, wb@(Box (c2, w2, h2))) =
      if aabb b wb
      then cmap $ \(Player, Position p@(V2 p1 p2), Velocity v@(V2 v1 v2)) ->  case (\z1 z2 -> aabb (box (c + V2 (z1*v1) (z2*v2)) w h) wb) <$> [1,-1] <*> [-1,1] of
         [False,_,_,_] -> (Player, Position $ p + V2 v1 (negate v2), Velocity (V2 v1 (negate v2) / 5))
         [_,False,_,_] -> (Player, Position $ p + v, Velocity (v / 5))
         [_,_,False,_] -> (Player, Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) (negate v2) / 5))
         [_,_,_,False] -> (Player, Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) v2 / 5))
         
         [True,True,True,True] -> (Player, Position $ p + negate v, Velocity $  negate (v/5))
         otherwise  -> (Player, Position $ p + negate v, Velocity (negate v / 5))
         
      else return () 
  
