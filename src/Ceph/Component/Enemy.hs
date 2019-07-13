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

enemyLoop :: V2 Float -> System World ()
enemyLoop p1 = do
      conceIfM_
        (\(Enemy1,Charge c _) -> c >= 1)
        (\(Enemy1, v, p, e) -> shootBullet (Target p1) p v (Charge 1 False) >> e `set` (Charge 0 True,go2player p1 p v))
      cmapM_ $ \case
        (Arrow, b) -> cmap (hurtEnemy b)
        (_,_) -> return ()
      cmap killEnemy
      cmapIf (\(Enemy1, Charge c _) -> c <= 2) (\(Enemy1, Charge c _, v, p) -> (Charge (c + 0.03) True, go2player p1 p v))
      cmapIf (\(Enemy1, Charge c _) -> c >= 2) (\(Enemy1, Charge c _, v, p) -> (Charge 0 True, go2player p1 p v))
    
killEnemy :: (Enemy, Health, Sprites) ->  (Enemy, Health, Animated, Sprites)
killEnemy e@(Enemy1, h, s@(Sprites (p:_)))
  | h <= 0 = (Enemy1, 0, Animate 1800, Sprites (replicate 60 p ++ replicate 60 (Color red $ Circle 1)))
  | True = (Enemy1, 0, Still, s)

hurtEnemy :: (Box,Projectile) -> (Box, Health, Enemy) -> Health
hurtEnemy ((Box painBox),Arrow) (Box enemyBox, h, _)  = if (aabb (Box painBox) (Box enemyBox)) then (h - 1) else h
hurtEnemy (_,_) (_, h, _)  = h

go2player :: V2 Float -> Position -> Velocity -> Velocity
go2player m (Position p) v 
      | (norm (m - p) < 50) = ( v + Velocity (0.006 * (normalize $ m - p)))
      | (norm (m - p) < 180) = ( v + Velocity (0.009 * (normalize $ m - p)))
      | (norm (m - p) < 460) = ( v + Velocity (0.008 * (normalize $ m - p)))
      | (norm (m - p) < 1068) = ( v + Velocity (0.07 * (normalize $ m - p)))
      | True = v
 
enemy :: Picture -> (Music Pitch,M.Chunk) -> System World ()
enemy s (am,cm) = do
  g <- liftIO $ randomRIO (1, 3 :: Float)
  [n,o,p,q] <- liftIO $ replicateM 4 $ randomRIO (-100, 100 :: Int)
  let bp =  Rotate (pi/2) . Scale (0.01*g) (0.01*g) $ s
  newEntity (
    (Enemy1, Enemy)
    , Charge g (g <= 2)
    , ( BodyPicture bp,
        Position (fromIntegral <$> V2 p q),
        Velocity 0,
        Box (fromIntegral <$> V2 p q, 1, 1),
        Angle 0)
    , Sprites [bp] -- (replicate 60 bp ++ replicate 60 (Color red $ Circle 1))
    , ( ProjCount 3,
      Attack,
      Health 1,
      SFXResources [cm] am)
    )
  return ()
