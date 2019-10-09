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
import Foreign.C.Types
import Graphics.Gloss
import qualified SDL.Mixer as M
import Linear

enemyLoop :: V2 CDouble -> System World ()
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

go2player :: V2 CDouble -> Position -> Velocity -> Velocity
go2player m (Position p) v 
      | (norm (m - p) < 500) = ( v + Velocity (0.0000001 * (normalize $ m - p)))
      | (norm (m - p) < 1080) = ( v + Velocity (0.00000009 * (normalize $ m - p)))
      | (norm (m - p) < 4060) = ( v + Velocity (0.00000008 * (normalize $ m - p)))
      | (norm (m - p) < 10068) = ( v + Velocity (0.00000007 * (normalize $ m - p)))
      | True = v
 
enemy :: Txtr -> System World ()
enemy s = do
  g <- liftIO $ randomRIO (1, 3 :: CDouble)
  nopq <- liftIO $ replicateM 4 $ randomRIO (-100, 100 :: Int)
  let [n,o,p,q] = nopq
  newEntity (
    (Enemy1, Enemy)
    , Charge g (g <= 2)
    , ( BodyPicture s,
        Position (fromIntegral <$> V2 p q),
        Velocity 0,
        Gravity (V2 0 (-1)),
        Box (fromIntegral <$> V2 p q, 1, 1),
        Angle 0)
    , ( ProjCount 3,
      Attack,
      Health 1,
      SFXResources [] [])
    )
  return ()
