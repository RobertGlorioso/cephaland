{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Ceph.Component.Enemy where

import Ceph.Util
import Ceph.Components
import Ceph.Component.Projectile
import Ceph.Physics.Box

import Apecs
import Apecs.System
import System.Random
import Foreign.C.Types
import qualified SDL as S
import Linear

enemyLoop :: (Box,Velocity,Entity) -> System World ()
enemyLoop eb@(Box (p1,_,_),_,_) = do 
  conceIfM_
    (\(Enemy1,Charge c _) -> c >= 10)
    (\(Enemy1, v, p, e) -> shootBullet (Target p1) p v (Charge 1 False) >> e `set` (Charge 0 True, acc2player p1 p v))
  cmapM_ $ \case
    (Arrow, b) -> cmap (hurtEnemy b)
    (_,_) -> return ()
  cmapIf (\(Enemy1, Charge c _) -> c <= 50000) (\(Enemy1, Charge c _, v, p) -> (Charge (c + 0.1) True, acc2player p1 p v))
  cmapIf (\(Enemy1, Charge c _) -> c >= 50000) (\(Enemy1, Charge _ _, v :: Velocity) -> (Charge 0 True, Velocity (pure 0.01) * v))
  cmap $ \case
    (Enemy, Trapped, _) -> (Out, Position $ pure 2e10)
    (_,_,i) -> i

hurtEnemy :: (Box,Projectile) -> (Box, Health, Enemy) -> Health
hurtEnemy ((Box painBox),Arrow) (Box enemyBox, h, _)  = if (aabb (Box painBox) (Box enemyBox)) then (h - 1) else h
hurtEnemy (_,_) (_, h, _)  = h

acc2player :: V2 CDouble -> Position -> Velocity -> Velocity
acc2player m (Position p) v@(Velocity vel) 
      | norm vel > enemySpeedLimit = v
      | (norm (m - p) < 500) = ( v + Velocity (0.01 * ( normalize $ m - p )))
      | (norm (m - p) < 1080) = ( v + Velocity (0.001 * ( normalize $ m - p )))
      | (norm (m - p) < 4060) = ( v + Velocity (0.0008 * ( normalize $ m - p )))
      | (norm (m - p) < 10068) = ( v + Velocity (0.0001 * ( normalize $ m - p )))
      | True = v
      where enemySpeedLimit = 10

go2player :: V2 CDouble -> Position -> Velocity -> Velocity
go2player m (Position p) v 
      | (norm (m - p) < 500) = ( Velocity (4 * ( normalize $ m - p )))
      | (norm (m - p) < 1080) = ( Velocity (2 * ( normalize $ m - p )))
      | (norm (m - p) < 4060) = ( Velocity ( ( normalize $ m - p )))
      | (norm (m - p) < 10068) = ( Velocity ( ( normalize $ m - p )))
      | True = v
  
enemy :: S.Renderer -> FilePath -> Enemy -> SFXResources -> System World Entity
enemy r enmFile enm sfx = do
  txtr <- liftIO $ loadTxtr r enmFile
  (p,q) <- liftIO $ (,) <$> randomRIO (-200,700) <*> randomRIO (-700,200)
  newEntity (
    (Enemy, enm)
    , Charge 0 False
    , ( txtr,
        Position (V2 p q),
        Velocity 0,
        Gravity (V2 0 0),
        Box (V2 p q, 20, 30),
        Angle 0)
    , ( ProjCount 3,
      Attack,
      Health 1,
      sfx
    ))