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

killEnemy painBox (Enemy, Box enemyBox, e) =
  if aabb (Box painBox) (Box enemyBox)
  then e `set` Position ( pure 12000 )
  else return ()


hurtPlayer :: Box -> (Enemy, Box, Entity) -> System World ()
hurtPlayer (Box playerBox) (Enemy, Box enemyBox, e) =
  if aabb (Box playerBox) (Box enemyBox)
  then do
    cmap $ \(Player, Health e) -> (Player, Health $ e - 5)
  else return ()

shootPlayer :: (Enemy, Velocity, Position, Behavior) -> System World () 
shootPlayer (Enemy, v, p2, b) = do
  (Player,Position p1) <- head <$> getAll
  shootBullet (Target p1) p2 v (Charge 1 False)
  return ()

goToPlayer :: V2 Float -> (Enemy, Velocity, Position) -> (Enemy, Velocity)
goToPlayer m (Enemy, Velocity v, Position p)
  | (norm (m - p) < 68) = (Enemy, Velocity $ v + (0.008 * (normalize $ m - p)))
  | (norm (m - p) < 26) = (Enemy, Velocity $ v + (0.009 * (normalize $ m - p)))
  | (norm (m - p) < 18) = (Enemy, Velocity $ v + (0.014 * (normalize $ m - p)))
  | (norm (m - p) < 13) = (Enemy, Velocity $  v + (0.024 * (normalize $ m - p)))
  | True = (Enemy, Velocity v) 

randGoToPlayer ::  V2 Float  -> Entity -> (Enemy, Velocity, Position, Entity) -> System World ()
randGoToPlayer m (Entity e') (Enemy, Velocity v, Position p, Entity e) = do
  if (e == e') then (Entity e) `set` (Enemy, Velocity $ (pure $ 0.1 + norm v) * (normalize $ m - p)) else return () 
    
enemy :: Picture -> Music Pitch -> System World ()
enemy j s = do
  g <- liftIO $ randomRIO (0.1, 3 :: Float)
  [n,o,p,q] <- liftIO $ replicateM 4 $ randomRIO (-100, 100 :: Int)
  let i = toEnum (max 0 . min 127 $ sum [n,o,p,q])
  mzk <- M.decode . makeByteMusic $ instrument i s
  
  newEntity ( (Enemy, Charge 0.0 True, Weapon i)
            
            , ( BodyPicture . (Rotate $ (pi/2)) . Scale (0.02*g) (0.02*g) $ j
              , Velocity 0
              , Position ((*0.1).fromIntegral <$> V2 p q)
              , Angle 0
              )
            , (Resources [] [mzk]
              , ProjCount 3
              , Box ((fromIntegral <$> V2 p q), g, g)
              , Attack
              )
            )
  return ()
