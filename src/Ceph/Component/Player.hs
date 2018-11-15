module Ceph.Component.Player where

import Apecs
import Linear
import Control.Monad
import Ceph.Component.Projectile
import Ceph.Components

playerShoot :: (Charge, Position, Velocity, ProjCount, Player1, Entity) -> System World ()
playerShoot o@(c, x, v, ProjCount arrowsLeft, Player, e) = do
        t <- get global 
        --cmap $ \(ProjCount n, Player) -> (Player, ProjCount $ arrowsLeft - 1)
        when (arrowsLeft >= 1) $ do
          --cmapM_ $ \(Player,Resources _ _ p) -> if p == [] then return () else M.play $ head p
          shootArrow t x v c
          e `set` (Charge 0.25 False, x, v, ProjCount $ arrowsLeft - 1,  Player)


speedLimit = 7
movePlayer :: V2 Float -> (Player1, Velocity, Behavior) -> (Player1, Velocity, Behavior)
movePlayer v c@(Player, Velocity p, b :: Behavior) 
  | norm p > speedLimit = c
  | True = if b /= Plant then
             (Player, Velocity $ p + v, b ) else
             (Player, Velocity $ p + v, NoBehavior)
       

playerDash :: (Player1, Dash) -> System World ()
playerDash (Player, Dash w) =do
  if w >= 8.0
  then do
    cmapM_ $ \(Target o) -> cmap $ \(Player, Position p, _ :: Behavior) -> (NoBehavior, Velocity $ 2 *  normalize (o - p))
    cmap $ \(Dash w) -> Dash (-8.0)
  else return ()
  cmap $ \(Player) -> (Player, Attacking)
               
