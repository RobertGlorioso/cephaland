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
import Control.Monad
import System.Random
import Foreign.C.Types
import qualified SDL.Mixer as M
import Linear

enemyLoop :: V2 CDouble -> System World ()
enemyLoop p1 = do
      conceIfM_
        (\(Enemy1,Charge c _) -> c >= 10)
        (\(Enemy1, v, p, e) -> shootBullet (Target p1) p v (Charge 1 False) >> e `set` (Charge 0 True,go2player p1 p v))
      cmapM_ $ \case
        (Arrow, b) -> cmap (hurtEnemy b)
        (_,_) -> return ()
      cmapIf (\(Enemy1, Charge c _) -> c <= 20) (\(Enemy1, Charge c _, v, p) -> (Charge (c + 0.03) True, go2player p1 p v))
      cmapIf (\(Enemy1, Charge c _) -> c >= 20) (\(Enemy1, Charge c _, v, p) -> (Charge 0 True, go2player p1 p v))

hurtEnemy :: (Box,Projectile) -> (Box, Health, Enemy) -> Health
hurtEnemy ((Box painBox),Arrow) (Box enemyBox, h, _)  = if (aabb (Box painBox) (Box enemyBox)) then (h - 1) else h
hurtEnemy (_,_) (_, h, _)  = h

go2player :: V2 CDouble -> Position -> Velocity -> Velocity
go2player m (Position p) v 
      | (norm (m - p) < 500) = ( v + Velocity (0.1 * ( normalize $ m - p)))
      | (norm (m - p) < 1080) = ( v + Velocity (0.09 * ( normalize $ m - p)))
      | (norm (m - p) < 4060) = ( v + Velocity (0.08 * ( normalize $ m - p)))
      | (norm (m - p) < 10068) = ( v + Velocity (0.07 * ( normalize $ m - p)))
      | True = v
 
enemy :: Txtr -> (M.Chunk,M.Chunk) -> System World ()
enemy txtr (m1,m2) = do
  g <- liftIO $ randomRIO (1, 3 :: CDouble)
  nopq <- liftIO $ replicateM 4 $ randomRIO (-100, 100 :: Int)
  ranColor <- liftIO $ (\(r:g:b:a:_) -> return $ V4 r g b a) =<< replicateM 4 ( randomRIO (0,255) )
  
  let [n,o,p,q] = nopq
  newEntity (
    (Enemy1, Enemy)
    , Charge g (g <= 2)
    , ( txtr,
        Position (fromIntegral <$> V2 p q),
        Velocity 0,
        Gravity (V2 0 0),
        Box (fromIntegral <$> V2 p q, 1, 1),
        Angle 0)
    , ( ProjCount 3,
      Attack,
      Health 1,
      SpriteColor ranColor,
      SFXResources m1 m2 [])
    )
  return ()
