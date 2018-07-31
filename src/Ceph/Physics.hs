{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Ceph.Physics where

import Ceph.Util
import Ceph.Physics.Box
import Ceph.Components
import Ceph.Entity.Sword
import Ceph.Entity.Enemy

import Apecs
import Apecs.Util
import Apecs.Concurrent
import Control.Concurrent
import Graphics.Gloss
import System.Random
import Linear

stepper dT w = runWith w ( cmapM_ mainPlayerLoop ) >> return w
  where
     
    mainPlayerLoop (Player, Position p1) = do
      stepPhysics (1/60)
      cmapM_ $ \(Player, Charging) -> do
        cmap $ \(Charge c) -> (Charge $ c + 0.01)
      cmapM_ $ \(Dash x) -> if x < 8.0 then cmap $ \(Dash x') -> Dash (x' + 0.3) else return ()
        
      cmapM_ $ \(Projectile, Velocity v, Box pb) -> if norm v > 0.4 then cmapM_ $ killEnemy pb else return ()
      
      l <- return . length =<< ( getAll :: System World [Enemy]) 
      s <- liftIO $ randomRIO (0, l*5)
      
      [(Player, Position p2@(V2 x1 y1))] <- getAll
      

      cmapM_ $ goToPlayer p2 (Entity s)
      cmap $ \(Target o) -> Target ( o + p2 - p1 )
      Target tp@(V2 x2 y2) <- get global
      
      cmap $ \(Player) -> (Angle (vToRad $ p2 - tp))
      cmapM_ $ \(Player, _ :: Not Attacking) -> cmap hideSword
      cmapM_ $ \(Player, Attacking) -> do  
        cmapM_ $ \(Sword, Box sb) -> do
          cmap $ showSword x1 x2 tp p2
          cmapM_ $ killEnemy sb
        
          --cmapM_ $ \(Projectile, Velocity (V2 x y), Position p, e) -> if (x <= 0.005 && y <= 0.005) then e `destroy` (Proxy :: Proxy Position) else return ()

stepPhysics :: Double -> System World ()
stepPhysics dT = do
  --moves all objects that are in motion (have a Velocity)
  Gravity g <- get global
  cmap $ \(Position p, Velocity v) -> (Position $ p + v, Velocity $ v + g)
  
  --correct angle for arrows
  cmap $ \(Velocity v, Angle t, Projectile) -> (Projectile, Angle $ vToRad v + pi / 2)
        
  --updates all hit boxes to current position
  cmap $ \(Box (b, w, h), Position p) -> (Box (p, w, h))
  boxBound
