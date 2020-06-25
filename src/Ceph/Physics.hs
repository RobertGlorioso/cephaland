{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}


module Ceph.Physics where

import Ceph.Jams
import Ceph.Util
import Ceph.Components
import Ceph.Handler
import Ceph.Component.Player
import Ceph.Component.Enemy
import Ceph.Component.Levels
import Ceph.Component.Weapon
import Ceph.Component.Projectile
import Ceph.Physics.Box
import Control.Monad
import Apecs
import Linear
import SDL.Input
import Data.Bool
          
physicsStep :: World -> IO ()
physicsStep !w = runWith w $ do
  ks <- getKeyboardState
  ms <- getMouseButtons
  handle ks ms
  get global >>= \b -> when (status b == Play) incrementBeat

  cmapM playerLoop
  
  --cmapM_ $ \pc@(Box (p,_,_), _, Player1) -> do
  --  cmap $ squallBounce pc
  --  randomizeGridCell (Position p)
  --  enemyLoop p
  
  motion
  where
    motion = do
      cmap $ \case
        (b, p, _, _, Plant, f) -> (b,p,0,Plant,f)
        (Box (_,w,h), Position p, Velocity v, _, s, Weapon)
          -> (Box (p+v,w,h), Position $ p + v, Velocity v, s, Weapon)
        (Box (_,w,h), Position p, Velocity v, _, Seek, f)
          -> (Box (p+v,w,h), Position $ p + v, Velocity v, Seek, f)
        (Box (_,w,h), Position p, Velocity v, Gravity g, s, Projectile) 
          -> (Box (p+v,w,h), Position $ p + v, Velocity $ g + v, s, Projectile)
        (Box (_,w,h), Position p, Velocity v, Gravity g, e, Player) 
          -> (Box (p+v,w,h), Position $ p + v,
              if norm v < playerSpeedLimit 
              then Velocity $ v + g 
              else Velocity $ pure playerSpeedLimit * normalize (v + g)
              , e, Player)
        (Box (_,w,h), Position p, Velocity v, Gravity g, e, f) 
          -> (Box (p+v,w,h), Position $ p + v, Velocity $ v + g, e, f)
      
      {--cmapM $ \(Orbiting ent, Box (_,w,h)) -> do
        (Position x, Velocity v) <- get ent
        return $ (Position $ x - pure 100, Box (x - pure 100,w,h))--}

      --correct angle for certain moving objs
      cmap $ \(Velocity v, Angle n, a ) -> if a == Enemy then (Angle $ v2ToRad v) else Angle n
      
      cmap $ \(AngularMomentum m, Angle n) -> Angle $ n + m
      --move chains and other linked objs
      cmapM moveChains
      cmapM moveNets
      --updates scope for rendering & collision detection
      (Camera cam _) <- get global :: System World Camera
      cmap (\(_::Actor,b) -> bool Out In $ aabb b (Box (cam, 600, 600)) )

      --physics for colliding with walls
      boxBound
      netLoop
      
