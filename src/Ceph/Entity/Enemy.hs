{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Ceph.Entity.Enemy where

import Ceph.Util
import Ceph.Components
import Ceph.Physics.Box

import Apecs
import Apecs.Util
import Control.Monad
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Linear

killEnemy painBox (Enemy, Box enemyBox, e) =
  if aabb (Box painBox) (Box enemyBox)
  then e `set` Position ( pure 2000 )
  else return ()

hurtPlayer :: Box -> (Enemy, Box, Entity) -> System World ()
hurtPlayer (Box playerBox) (Enemy, Box enemyBox, e) =
  if aabb (Box playerBox) (Box enemyBox)
  then do
    cmap $ \(Player, Health e) -> (Player, Health $ e - 5)
  else return ()

goToPlayer :: V2 Float -> (Enemy, Velocity, Position, Behavior) -> (Enemy, Velocity, Behavior)
goToPlayer m (Enemy, Velocity v, Position p, b) = if (norm (m - p) < 26) then (Enemy, Velocity $ 0.05 * (normalize $ m - p), NoBehavior) else (Enemy, Velocity v, b)

randGoToPlayer ::  V2 Float  -> Entity -> (Enemy, Velocity, Position, Entity) -> System World ()
randGoToPlayer m (Entity e') (Enemy, Velocity v@(V2 e1 e2), Position p, Entity e) = do
  if (e == e') then (Entity e) `set` (Enemy, Velocity $ (pure $ 0.1 + norm v) * (normalize $ m - p)) else return () 
    
enemy :: System World ()
enemy = do
  g <- liftIO $ randomRIO (0.1, 3 :: Float)
  [n,o,p,q] <- liftIO $ replicateM 4 $ randomRIO (-80, 80 :: Float)
  newEntity ( Enemy
              , BodyPicture . color yellow $ Circle g
              , Velocity (V2 n o)
              , Position (V2 p q)
              , Angle 0
              , ProjCount 3
              , Box ((V2 p q), g, g)
              , Seek
              )
  return ()
