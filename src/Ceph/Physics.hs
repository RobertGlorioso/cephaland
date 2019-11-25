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
import Ceph.Scene
import Ceph.Components
import Ceph.Handler
import Ceph.Component.Player
import Ceph.Component.Enemy
import Ceph.Component.Levels
import Ceph.Physics.Box
import Apecs
import Linear
import SDL.Input
import Data.Bool
          
physicsStep :: World -> IO ()
physicsStep !w = runWith w $ do
  getKeyboardState >>= handle
  incrementBeat w
  cmapM playerLoop
  pp  <- cfoldM (\a b -> return (b:a) ) []
  let [(Player1, Position p)] = pp
  randomizeGridCell (Position p)
  enemyLoop p
  motion
  where
    motion = do
      cmap $ \case
        c@( b, p, _, _, Plant, f) -> (b,p,0,Plant,f)
        (Box (_,w,h), Position p, Velocity v, _, s, Weapon) -> (Box (p+v,w,h), Position $ p + v, Velocity v, s, Weapon)
        (Box (_,w,h), Position p, Velocity v, _, Seek, f) -> (Box (p+v,w,h), Position $ p + v, Velocity v, Seek, f)
        (Box (_,w,h), Position p, Velocity v, Gravity g, s, Projectile) -> (Box (p+v,w,h), Position $ p + v, Velocity $ 0.1 * g + v, s, Projectile)
        (Box (_,w,h), Position p, Velocity v, Gravity g, Moving v2, f) -> (Box (p+v,w,h), Position $ p + v, if norm v < speedLimit then Velocity $ v + v2 + g else Velocity $ pure (0.9*speedLimit) * normalize v, Moving v2, f)
        (Box (_,w,h), Position p, Velocity v, Gravity g, e, Player) -> (Box (p+v,w,h), Position $ p + v, if norm v < speedLimit then Velocity $ v + g else Velocity $ pure speedLimit * normalize (v + g), e, Player)
        (Box (_,w,h), Position p, Velocity v, Gravity g, e, f) -> (Box (p+v,w,h), Position $ p + v, Velocity $ v + g, e, f)

      --correct angle for certain moving objs
      cmap $ \(Velocity v, Angle t, a ) -> if a == Enemy || a == Projectile then (Angle $ v2ToRad v) else Angle t
      cmapM $ \case
        (Linked e f, Chain) -> do
          (Position p1) <- get e
          (Position p0) <- get f
          return $ (Angle $ v2ToRad (p0 - p1), Position $ (p0 + p1) / 2, box ((p0 + p1) / 2) 0.05 0.05)
        (WLinked e f m, Chain) -> do
          (Position p0) <- get e
          (Position p1) <- get f
          return $ (Angle $ v2ToRad (p0 - p1), Position $ (pure (1-m) * p0 + (pure m * p1)), box ((pure (1-m) * p0 + (pure m * p1))) 0.05 0.05)

      --updates scope for rendering & collision detection
      (Camera cam _) <- get global :: System World Camera
      cmap (\b -> bool Out In $ aabb b (Box (cam, 600, 600)))

      --physics for colliding with walls
      boxBound
      
