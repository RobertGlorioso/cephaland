{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
--{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
-- {-# OPTIONS_GHC -fno-warn-orphans                     #-}
-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}


module Ceph.Physics where

import Ceph.Util
import Ceph.Jams
import Ceph.Physics.Box
import Ceph.Components
import Ceph.Component.Projectile
import Ceph.Component.Sword
import Ceph.Component.Enemy

import Apecs
import Apecs.Util
--import qualified Data.Vector.Sized                   as V
--import qualified Data.Vector.Generic.Sized           as VG
--import Numeric.LinearAlgebra.Static hiding (dim, (<>))
--import Numeric.LinearAlgebra.Static.Vector
import Control.Concurrent
import Control.Monad
import qualified Data.IntMap as M
import Graphics.Gloss
import System.Random
--import Numeric.Hamilton
import Linear

moveStuff :: V2 Float -> Entity -> Float -> System World ()
moveStuff r e a = do
  [o,p] <- liftIO $ fmap (\m -> signum m * a + m) <$> replicateM 2 (randomRIO (-20,20))
  e `set` Position (r + V2 o p)
  e `set` Velocity 0
          
stepper :: Float -> World -> IO World
stepper _ !w = runWith w $ do
  incrementBeat w
  [(Player, Position p1)] <- getAll
  cmapM actorLoop1
  motion
  cmapM $ actorLoop2 p1
  return w
  where
    incrementBeat w = do
      (Beat m i) <- get global
      -- plays sound effects on beat
      if (m == i) then (global `set` Beat m 0) >> cmapM_ ( \case
        (Sing,Song i, a :: Actor, e) ->  e `set` (Debug . show $ (a, Song i), NoBehavior) >> playSong w e
        _ -> return () 
        )
        else global `set` Beat m (i+1) 

-------------------------------------------------------------------
    motion = do
      Gravity g <- get global
      --moves all objects that are in motion (have a Velocity and Box )
      forkSys . atomically . cmap $ \case
        c@(Box _, Position p, Velocity v, Plant) -> c
        (Box (_,w,h), Position p, Velocity v, Seek) -> (Box (p+v,w,h), Position $ p + v, Velocity v, Seek)
        (Box (_,w,h), Position p, Velocity v, e) -> (Box (p+v,w,h), Position $ p + v, Velocity $ 0.999 * ( v + g ), e)
      --correct angle for certain moving objs
      cmap $ \ (Velocity v, Angle t, a ) -> if a == Enemy1 || a == Projectile then (Angle $ v2ToRad v) else Angle t
      --physics for colliding with walls
      boxBound

--------------------------------------------------------------------
    actorLoop1 :: (Player1, Dash, Box, Behavior, Charge, Entity) -> System World ()
    
    actorLoop1 (Player, Dash dx, b@(Box (p1@(V2 x1 y1),_,_)), c, Charge cv chging, e) = do
      liftIO . print $ c
      --allows the player to pick up an enemy or other entity
      when ( c == Carry ) $ do
        e <- return . filter (\(p, _, a) -> aabb b p && a == Wall) =<< (getAll :: System World [(Box, Entity, Actor)])
        if length e == 0 then return () else do
          let (_,carriedEnt,_) = head e
          carriedEnt `set` (Position p1)

      Target tp@(V2 x2 y2) <- get global
      cmapM $ goToPlayerAndShoot p1
      --cmapM_ $ hurtPlayer b
      let chg
            | chging = if cv < 10 then (Charge (cv + 0.005) True) else (Charge cv False)
            | True   = Charge cv False
      let dsh
            | dx < 8.0 = Dash (dx + 0.3)
            | True     =  Dash dx
                                                                  
      e `set` (Player, Angle (v2ToRad $ p1 - tp), chg, dsh)
        
      --cmapM_ $ \(Player, _ :: Not Attacking) -> cmap hideSword
      --cmapM_ $ \(Player, Attacking) -> do  
      --  cmapM_ $ \(Sword, Box sb) -> do
      --    cmap $ showSword x1 x2 tp p1
      --    cmapM_ $ killEnemy sb
      
      
      
      
      --a cooldown timer for the dash action  
    actorLoop2 :: V2 Float -> (Position,Player1) -> System World ()
    actorLoop2 p1 (Position p2@(V2 x1 y1),Player) = do
      --cmap checkProj
      
      cmap $ \(Target o) -> Target (o + p2 - p1)
      
      cmapM_ $ \(Grid is) -> do
        let (floor -> gx) = (x1 / 500) + (signum x1)
            (floor -> gy) = (y1 / 500) + (signum y1)
            updateGrid g = cmap $ \(Grid _) -> Grid g
            moveEnemyWalls =
              cmapM_ $ \case
                (Wall,Position wp, e) -> when (norm (wp - p2) > 1000) $ moveStuff p2 e 200
                (Enemy1,Position ep, e) -> when (norm (ep - p2) > 1000) $ moveStuff p2 e 200
                (_,_,_) -> return ()
                                    
        if length is > 10 then updateGrid mempty else return ()
        case M.lookup gx is of
          Just ys -> if gy `elem` M.keys ys then return ()
                     else updateGrid (M.insert gx (M.insert gy () ys) is) >> moveEnemyWalls
          Nothing -> moveEnemyWalls >> updateGrid (M.insert gx mempty is)
                    

      --}
      --we can implement a clamp like so
      --cmap $ \(Position (V2 o p)) -> (Position $ V2 (max (-9e6) $ min 9e6 o) (min 9e6 $ max (-9e6) p))
        

      
      


{--
type HSystem = Numeric.Hamilton.System


pattern Z1 :: a -> V.Vector 1 a
pattern Z1 x <- (V.head->x)
  where
    Z1 x = V.singleton x
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Z1 #-}
#endif

type Z2 = V.Vector 2
pattern Z2 :: a -> a -> V.Vector 2 a
pattern Z2 x y <- (V.toList -> [x,y])
  where
    Z2 x y = V.fromTuple (x, y)
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Z2 #-}
#endif

pattern Z3 :: a -> a -> a -> V.Vector 3 a
pattern Z3 x y z <- (V.toList->[x,y,z])
  where
    Z3 x y z = V.fromTuple (x, y, z)
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Z3 #-}
#endif

pattern Z4 :: a -> a -> a -> a -> V.Vector 4 a
pattern Z4 x y z a <- (V.toList->[x,y,z,a])
  where
    Z4 x y z a = V.fromTuple (x, y, z, a)
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Z4 #-}
#endif

pendSys :: HSystem 2 1
pendSys = mkSystem' (vec2 1 10 )     -- masses
              (\(Z1 θ)   -> Z2 (sin θ) (0.5 - cos θ))     -- coordinates
              (\(Z2 _ y) -> y                       )     -- potential

twoBodySys :: Double -> Double -> HSystem 4 2
twoBodySys m1 m2 = let mT = m1 + m2 in mkSystem (vec4 m1 m1 m2 m2) -- masses
                 -- positions are calculated assuming (0,0) is the center
                 -- of mass
                 (\(Z2 r θ) -> let r1 = r * realToFrac (-m2 / mT)
                                   r2 = r * realToFrac (m1 / mT)
                               in  Z4 (r1 * cos θ) (r1 * sin θ)
                                      (r2 * cos θ) (r2 * sin θ)
                 )                 -- coordinates
                 (\(Z2 r _) -> - realToFrac (m1 * m2) / r)  -- potential
  
draw :: R 2 -> V2 Float
draw xs = (\(Z2 x y) -> realToFrac <$> V2 x y) $ VG.convert (rVec xs)

draw2 :: R 4 -> (V2 Float,V2 Float)
draw2 xs = (\(Z4 x y w z) -> (realToFrac <$> V2 x y, realToFrac <$> V2 w z)) $ VG.convert (rVec xs)

dblPendPhs1 :: Double -> Double -> Double -> Phase 2
dblPendPhs1 m1 m2 w = toPhase (twoBodySys m1 m2) $ Cfg (vec2 2 0) (vec2 0 w)--(konst θ0 :: R 1) (konst ω0 :: R 1)

pendPhs1 :: Double -> Double -> Phase 1
pendPhs1 θ0 ω0 = toPhase pendSys $ Cfg (konst θ0 :: R 1) (konst ω0 :: R 1)

updateOneBody c@(Position p, PHS2 s, b, OneBody q) =
  if b /= Plant
  then let (mq, mr) =  draw2 . underlyingPos (twoBodySys 0.5 5) . phsPositions $ s in 
       ( Position $ q + 10 * mq
       , PHS2 $ stepHam (1/60) (twoBodySys 0.5 5) s
       , b
       , OneBody q)
  else c

updateGhost c@(_, _, Plant, _) = c
updateGhost (Position p, PHS s, b, Pend q) =
  (Position $ 10 * (draw . underlyingPos pendSys . phsPositions $ s) + q, PHS $ (stepHam (1/60) pendSys) s, b, Pend q)
updateGhost (Position p, PHS2 s, b, OneBody q) = let (mq, mr) =  draw2 . underlyingPos (twoBodySys 0.5 5) . phsPositions $ s in
       ( Position $ 10 * mq + q
       , PHS2 $ stepHam (1/60) (twoBodySys 0.5 5) s
       , b
       , OneBody q)
updateGhost (Position p, PHS2 s, b, TwoBody q) = let (mq, mr) =  draw2 . underlyingPos (twoBodySys 0.5 5) . phsPositions $ s in
       ( Position $ 10 * mr + q
       , PHS2 $ stepHam (1/60) (twoBodySys 0.5 5) s
       , b
       , TwoBody q)

setGhost :: V2 Float -> Ghost -> Ghost
setGhost v (OneBody _) = OneBody v
setGhost v (TwoBody _) = TwoBody v
setGhost v (Pend _) = Pend v
--}

