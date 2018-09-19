{-# LANGUAGE CPP #-}
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
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# OPTIONS_GHC -fno-warn-orphans                     #-}
-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}s


module Ceph.Physics where

import Ceph.Util
import Ceph.Physics.Box
import Ceph.Components
import Ceph.Entity.Projectile
import Ceph.Entity.Sword
import Ceph.Entity.Enemy

import Apecs
import Apecs.Util
import qualified Data.Vector.Sized                   as V
import qualified Data.Vector.Generic.Sized           as VG
import Numeric.LinearAlgebra.Static hiding (dim, (<>))
import Numeric.LinearAlgebra.Static.Vector
import Control.Concurrent
import Control.Monad
import GHC.TypeLits
import Graphics.Gloss
import System.Random
import Numeric.Hamilton
import Linear


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

carryEnt :: (Player,Box,Behavior) -> Apecs.System World ()
carryEnt (Player,p@(Box (x,_,_)),c) = when ( c == Carry ) $ do
  e <- return . filter (\(b, _, _, _) -> aabb b p ) =<< (getAll :: Apecs.System World [(Box, Entity, Not Wall, Not Player)])
  if length e == 0 then return () else do
    let (_,carriedEnt,_,_) = head e
    e <- exists carriedEnt (Proxy :: Proxy Ghost)
    when e $ do
      carriedEnt `set` Plant
      g <- get carriedEnt 
      carriedEnt `set` (setGhost x g)
      
    carriedEnt `set` (Position x)
  
stepper dT w = runWith w ( cmapM_ mainPlayerLoop ) >> return w
  where
    
    mainPlayerLoop (Player, (Box b@(p1,_,_), Velocity v)) = do

      --moves all objects that are in motion (have a Velocity)
      Gravity g <- get global
      cmap $ \c@(Position p, Velocity v, b :: Behavior) -> if b /= Plant then (Position $ p + v, Velocity $ v + g, b) else c
      
      --liftIO . print $ "updating ghosts"
      cmap updateGhost
      
      --liftIO $ print "correct angle for arrows"
      cmap checkArrow

      --liftIO $ print "updates all hit boxes to current position"
      forkSys . atomically . cmap $ \(Box (b, w, h), Position p) -> (Box (p, w, h))
      cmapM_ carryEnt
      boxBound

      cmapM_ $ \(Player, Charging) -> do
        cmap $ \(Charge c) -> (Charge $ c + 0.005)
      cmapM_ $ \(Dash x) -> if x < 8.0 then cmap $ \(Dash x') -> Dash (x' + 0.3) else return ()
        
      cmapM_ $ \(Projectile, Velocity v, Box pb) -> if norm v > 0.4 then cmapM_ $ killEnemy pb else return ()
      
      [(Player, Position p2@(V2 x1 y1))] <- getAll

      cmap $ goToPlayer p2
      cmapM_ $ hurtPlayer (Box b)
      cmap $ \(Target o) -> Target ( o + p2 - p1 )
      Target tp@(V2 x2 y2) <- get global
      
      cmap $ \(Player) -> (Angle (vToRad $ p2 - tp))
      cmapM_ $ \(Player, _ :: Not Attacking) -> cmap hideSword
      cmapM_ $ \(Player, Attacking) -> do  
        cmapM_ $ \(Sword, Box sb) -> do
          cmap $ showSword x1 x2 tp p2
          cmapM_ $ killEnemy sb
        
