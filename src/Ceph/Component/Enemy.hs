{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Ceph.Component.Enemy where

import Ceph.Util
import Ceph.Jams
import Ceph.Components
import Ceph.Component.Projectile
import Ceph.Physics.Box

import Apecs
import Apecs.Util
import Euterpea
import Control.Monad
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Juicy
import qualified SDL.Mixer as M
import Linear

killEnemy :: (V2 Float, Float, Float) -> (Enemy, Box, Entity) -> System World ()
killEnemy painBox (Enemy1, Box enemyBox, e) =
  if aabb (Box painBox) (Box enemyBox)
  then e `set` Position ( pure 2e7 )
  else return ()

hurtPlayerEnm :: Box -> (Actor, Box, Entity) -> System World ()
hurtPlayerEnm (Box playerBox) (a, Box enemyBox, e) =
  if a == Enemy && aabb (Box playerBox) (Box enemyBox)
  then do
    cmap $ \(Player1, Health e) -> (Player1, Health $ e - 5)
  else return ()


goToPlayerAndShoot :: V2 Float -> (Enemy, Velocity, Position, Charge, Entity) -> System World ()
goToPlayerAndShoot _ (Enemy1, _, _, Charge c False, e) = if c <= 1 then e `set` (Charge (c + 0.1) False) else e `set` (Charge 0 True)
goToPlayerAndShoot m (Enemy1, v, Position p, Charge c True, e) = do
  if c <= 1 then e `set` (Charge (c + 0.001) True, go2p) else shootBullet (Target m) (Position p) v (Charge 3 False) >> set e (Charge 0 False)
  where
    go2p 
      | (norm (m - p) < 680) = ( v + Velocity (0.007 * (normalize $ m - p)))
      | (norm (m - p) < 260) = ( v + Velocity (0.008 * (normalize $ m - p)))
      | (norm (m - p) < 180) = ( v + Velocity (0.010 * (normalize $ m - p)))
      | (norm (m - p) < 30) = ( v + Velocity (0.020 * (normalize $ m - p)))
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
      Resources [] [cm],
      Song am)
    )
  return ()
