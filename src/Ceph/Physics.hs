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
import Ceph.Component.Player
import Ceph.Component.Enemy
import Ceph.Physics.Box
import Apecs
import Control.Monad
import Data.Bool
import Data.Ord
import Data.List
import System.Random
          
stepper :: Float -> World -> IO World
stepper _ !w = runWith w $ do
  incrementBeat w
  [(Player1, Position p)] <- cfoldM (\a b -> return (b:a) ) [] 
  cmapM playerLoop
  enemyLoop p
  motion
  return w
  where
    motion = do
      Gravity g <- get global
      ls <- cfoldM (\a b -> return (b:a)) [] :: System World [(Linked,( Position, Entity))]
      cmap $ \case
        c@( _, _, _, Plant, _) -> c
        (Box (_,w,h), Position p, Velocity v, s, Weapon) -> (Box (p+v,w,h), Position $ p + v, Velocity v, s, Weapon)
        (Box (_,w,h), Position p, Velocity v, Seek, f) -> (Box (p+v,w,h), Position $ p + v, Velocity v, Seek, f)
        (Box (_,w,h), Position p, Velocity v, e, f) -> (Box (p+v,w,h), Position $ p + v, Velocity $ ( v + g ) * 0.999 , e, f)

      --correct angle for certain moving objs
      cmap $ \(Velocity v, Angle t, a ) -> if a == Enemy || a == Projectile then (Angle $ v2ToRad v) else Angle t
     
      cmapM $ \case
        (Linked e f, Chain) -> do
          --[x,y] <- liftIO $ replicateM 2 (randomRIO (0.1,1) :: IO Float)
          (Position p1) <- get e
          (Position p0) <- get f
          return $ (Angle $ v2ToRad (p0 - p1), Position $ (p0 + p1) / 2)
        (WLinked e f m, Chain) -> do
          (Position p0) <- get e
          (Position p1) <- get f
          return $ (Angle $ v2ToRad (p0 - p1), Position $ (pure (1-m) * p0 + (pure m * p1)))
      --updates scope for rendering & collision detection
      (Camera cam _) <- get global :: System World Camera
      cmap (\b -> bool Out In $ aabb b (Box (cam, 400, 400)))

      --physics for colliding with walls
      boxBound
      
