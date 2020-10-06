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
  cmapIf (\(Enemy1, Charge c _) -> c >= 10000) (\(Enemy1, Charge c _, v, p) -> (Charge 0 True, acc2player p1 p v))
  cmapIf (\(Enemy1, Charge c _) -> c >= 9000) (\(Enemy1, Charge c _, v :: Velocity, Position p) -> (Charge (c + 1) True, Velocity $ (pure 0.5) * (normalize $ p1 - p )))
  cmapIf (\(Enemy1, Charge c _) -> c <= 9000) (\(Enemy1, Charge c _, v :: Velocity, Position p) -> (Charge (c + 1) True))
  cmap $ \case
    (Enemy, Trapped, _) -> (Out, Position $ pure 2e10)
    (_,_,i) -> i

enemySpeedLimit :: CDouble
enemySpeedLimit = 10.8

hurtEnemy :: (Box,Projectile) -> (Box, Health, Enemy) -> Health
hurtEnemy ((Box painBox),Arrow) (Box enemyBox, h, _)  = if (aabb (Box painBox) (Box enemyBox)) then (h - 1) else h
hurtEnemy (_,_) (_, h, _)  = h

acc2player :: V2 CDouble -> Position -> Velocity -> Velocity
acc2player m (Position p) v@(Velocity vel) 
      | norm vel > enemySpeedLimit = v
      | (norm (m - p) < 500) = ( v + Velocity (0.1 * ( normalize $ m - p )))
      | (norm (m - p) < 1080) = ( v + Velocity (0.2 * ( normalize $ m - p )))
      | (norm (m - p) < 2060) = ( v + Velocity (1.5 * ( normalize $ m - p )))
      | (norm (m - p) < 4068) = ( v + Velocity (2.6 * ( normalize $ m - p )))
      | True = v

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