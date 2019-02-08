{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Ceph.Component.Enemy where

import Ceph.Util
import Ceph.Jams
import Ceph.Components
import Ceph.Component.Projectile
import Ceph.Physics.Box

import Apecs
import Apecs.System
import Euterpea
import Control.Monad
import System.Random
import Graphics.Gloss
import qualified SDL.Mixer as M
import Linear

killEnemy :: (Enemy, Health, Position) ->  (Enemy, Health, Position)
killEnemy e@(Enemy1, h, p)
  | h < 0 = (Enemy1, 0, Position 2e7)
  | True = e

hurtEnemy :: Box -> (Box, Health, Enemy) -> Health
hurtEnemy (Box painBox) (Box enemyBox, h, _)  = if (aabb (Box painBox) (Box enemyBox)) then (h - 1) else h

enemyLoop :: V2 Float -> System World ()
enemyLoop p1 = do
      conceIfM_
        (\(Enemy1,Charge c _) -> c >= 1)
        (\(Enemy1, v, p, e) -> shootBullet (Target p1) p v (Charge 3 False) >> e `set` (Charge 0 True,go2p p1 p v))

      cmapIf (\(Enemy1,Charge c _) -> c <= 2) (\(Enemy1, Charge c _, v, p) -> (Charge (c + 0.01) True, go2p p1 p v))
      cmapIf (\(Enemy1,Charge c _) -> c >= 2) (\(Enemy1, Charge c _, v, p) -> (Charge 0 True, go2p p1 p v))
    

go2p :: V2 Float -> Position -> Velocity -> Velocity
go2p m (Position p) v 
      | (norm (m - p) < 10680) = ( v + Velocity (0.07 * (normalize $ m - p)))
      | (norm (m - p) < 4600) = ( v + Velocity (0.008 * (normalize $ m - p)))
      | (norm (m - p) < 1800) = ( v + Velocity (0.09 * (normalize $ m - p)))
      | (norm (m - p) < 500) = ( v + Velocity (0.001 * (normalize $ m - p)))
      | True = v
    
enemy :: Picture -> (Music Pitch,M.Chunk) -> System World ()
enemy s (am,cm) = do
  g <- liftIO $ randomRIO (1, 3 :: Float)
  [n,o,p,q] <- liftIO $ replicateM 4 $ randomRIO (-100, 100 :: Int)
  newEntity (
    (Enemy1, Enemy)
    , Charge g (g <= 2)
    , ( BodyPicture . Rotate (pi/2) . Scale (0.05*g) (0.05*g) $ s,
        Position (fromIntegral <$> V2 p q),
        Velocity 0,
        Box (fromIntegral <$> V2 p q, 1, 1),
        Angle 0)
    , ( ProjCount 3,
      Attack,
      Health 100,
      Resources [] [cm],
      Song am)
    )
  return ()
