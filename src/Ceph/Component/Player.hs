module Ceph.Component.Player where

import Apecs
import Linear
import Control.Monad
import Graphics.Gloss
import Ceph.Component.Projectile
import Ceph.Components
import Euterpea


player :: Picture -> Music Pitch -> System World Entity
player p m = newEntity (( Position (V2 0 50)
                        , 0 :: Velocity
                        , BodyPicture $ Scale (1/4) (1/4) p 
                        , Box (0, 1, 1))
                       , (ProjCount 30, Health 99, Dash 0)
                       , Resources [] []
                       , Song m
                       , (Player1, Player)
                       , (NoBehavior, Charge 0.01 False))
  

playerShoot :: (Charge, Position, Velocity, ProjCount, Player, Entity) -> System World ()
playerShoot o@(c, x, v, ProjCount arrowsLeft, Player1, e) = do
        t <- get global 
        when (arrowsLeft >= 1) $ do
          shootArrow t x v c
          e `set` (Charge 1.0 False, ProjCount $ arrowsLeft - 1)


speedLimit = 7
movePlayer :: V2 Float -> (Player, Velocity, Behavior) -> (Player, Velocity, Behavior)
movePlayer v c@(d, Velocity p, b :: Behavior) 
  | norm p > speedLimit = c
  | True = if b /= Plant then
             (d, Velocity $ p + v, b ) else
             (d, Velocity $ p + v, NoBehavior)
       

playerDash :: (Player, Dash) -> System World ()
playerDash (Player1, Dash w) =do
  if w >= 8.0
  then do
    cmapM_ $ \(Target o) -> cmap $ \(Player1, Position p, _ :: Behavior) -> (NoBehavior, Velocity $ 2 *  normalize (o - p))
    cmap $ \(Dash w) -> Dash (-8.0)
  else return ()
  cmap $ \(Player1) -> (Player1, Attack)
               
