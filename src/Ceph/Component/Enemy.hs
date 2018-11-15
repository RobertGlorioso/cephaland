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


hurtPlayer :: Box -> (Actor, Box, Entity) -> System World ()
hurtPlayer (Box playerBox) (Enemy1, Box enemyBox, e) =
  if aabb (Box playerBox) (Box enemyBox)
  then do
    cmap $ \(Player, Health e) -> (Player, Health $ e - 5)
  else return ()

shootPlayer :: (Enemy1, Velocity, Position, Behavior) -> System World (Charge, Velocity) 
shootPlayer (Enemy, v, p2@(Position x), b) = do
  (Player,Position p1) <- head <$> getAll
  shootBullet (Target p1) p2 v (Charge 3 False)
  return (Charge 0 False, v + Velocity ( 0.1 * (normalize $ x - p1)))
--shootPlayer (_,v,_,_) = return (Charge 0 False, v)

goToPlayerAndShoot :: V2 Float -> (Enemy1, Velocity, Position, Charge, Entity) -> System World ()
goToPlayerAndShoot _ (Enemy, Velocity v, Position p, Charge c False,e) = if c < 1 then e `set` (Charge (c + 0.005) False) else e `set` (Charge 0 True)
goToPlayerAndShoot m (Enemy, Velocity v, Position p, Charge c True,e) = do
  if c < 1 then e `set` (Charge (c + 0.005) True, go2p) else get e >>= shootPlayer >>= set e
  where
    go2p 
      | (norm (m - p) < 680) = ( Velocity $ v + (0.007 * (normalize $ m - p)))
      | (norm (m - p) < 260) = ( Velocity $ v + (0.008 * (normalize $ m - p)))
      | (norm (m - p) < 180) = ( Velocity $ v + (0.010 * (normalize $ m - p)))
      | (norm (m - p) < 130) = ( Velocity $  v + (0.020 * (normalize $ m - p)))
      | True = (Velocity v) 
goToPlayerAndShoot m _ = return ()


randGoToPlayer ::  V2 Float  -> Entity -> (Actor, Velocity, Position, Entity) -> System World ()
randGoToPlayer m (Entity e') (Enemy1, Velocity v, Position p, Entity e) = do
  if (e == e') then (Entity e) `set` (Enemy, Velocity $ (pure $ 0.1 + norm v) * (normalize $ m - p)) else return () 
    
enemy :: Music Pitch -> Entity -> System World ()
enemy s e = do
  g <- liftIO $ randomRIO (0.1, 3 :: Float)
  [n,o,p,q] <- liftIO $ replicateM 4 $ randomRIO (-100, 100 :: Int)
  let i = toEnum (max 0 . min 127 $ sum [n,o,p,q])
  mzk <- M.decode . makeByteMusic $ instrument i s
  e `set` (Resources [] [mzk], Song s, Weapon i , Box ((fromIntegral <$> V2 p q), g, g), Attack, Position ((*0.1).fromIntegral <$> V2 p q))
  
