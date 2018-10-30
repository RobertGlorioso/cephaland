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
{-# LANGUAGE TypeSynonymInstances                     #-}
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

wall ::  V2 Float -> V2 Float -> Picture -> System World Entity
wall c (V2 w h) p = newEntity (Wall, Angle 0, Position c, Box (c, w, h), BodyPicture $ Scale (0.04 * w) (0.04 * h) p)

wave :: V2 Float -> V2 Float -> V2 Float -> Picture -> System World Entity
wave (V2 sclW sclH) center dim@(V2 w h) p = newEntity (Wall, Angle 0, Position center, Box (center, w, h), BodyPicture $ Scale (sclW*w) (sclH*h) p)

carryEnt :: (Player,Box,Behavior) -> Apecs.System World ()
carryEnt (Player,p@(Box (x,_,_)),c) = when ( c == Carry ) $ do
  e <- return . filter (\(b, _, _, _) -> aabb b p ) =<< (getAll :: Apecs.System World [(Box, Entity, Not Enemy, Not Player)])
  if length e == 0 then return () else do
    let (_,carriedEnt,_,_) = head e
    {--e <- exists carriedEnt (Proxy :: Proxy Ghost)
    when e $ do
      carriedEnt `set` Plant
      g <- get carriedEnt 
      carriedEnt `set` (setGhost x g)
    --}  
    carriedEnt `set` (Position x)
  
stepper :: Float -> World -> IO World
stepper t w = runWith w ( cmapM_ mainPlayerLoop ) >> return w
  where
    mainPlayerLoop (Player, (Box b@(p1,_,_), Velocity v)) = do
      --moves all objects that are in motion (have a Velocity)
      Gravity g <- get global
      cmap $ \case
        c@(Position p, Velocity v, Plant) -> c
        (Position p, Velocity v, Seek) -> (Position $ p + v, Velocity v, Seek)
        (Position p, Velocity v, b) -> (Position $ p + v, Velocity $ 0.999 *( v + g ), b)
        
      (Beat m i) <- get global
      --liftIO . print . length $ =<< 
      if (m == i) then (global `set` Beat m 0) >> cmapM ( \case
        (Sing,e) -> playTune e >> return (NoBehavior)
        (b,e) -> return b
        )
         else global `set` Beat m (i+1) 
      
      --correct angle for moving objs
      cmap $ \ (Velocity v, Angle _, _ :: Not Player, _ :: Not Wall) -> (Angle $ vToRad v)
      
      --if (norm v > 0.0001) then e `set` (Projectile, Angle $ vToRad v + pi / 2) else e `set` (Position $ pure 20000)


      --updates all hit boxes to current position
      forkSys . atomically . cmap $ \(Box (b, w, h), Position p) -> (Box (p, w, h))

      --allows the player to pick up an enemy or other entity
      cmapM_ carryEnt

      --physics for colliding with walls
      boxBound

      --holding a mouse button down will increase projectile speed
      cmap $ \case
        (Player,Charge c True) -> if c < 1 then (Charge (c + 0.005) True) else (Charge c False)
        (Player,Charge c False) -> (Charge c False)

      --a cooldown timer for the dash action  
      cmapM_ $ \(Dash x) -> if x < 8.0 then cmap $ \(Dash x') -> Dash (x' + 0.3) else return ()
        
      --cmapM_ $ \(Projectile, Velocity v, Box pb) -> if norm v > 0.4 then cmapM_ $ killEnemy pb else return ()
      
      [(Player, Position p2@(V2 x1 y1))] <- getAll
      cmap $ \(Target o) -> Target ( o + p2 - p1 )
      --we can implement a clamp like so
      --cmap $ \(Position (V2 o p), Enemy) -> (Position $ V2 o (max (-100) p))

      --this grid will keep track of what areas the player recently visited
      --it adds new walls and updates enemys if a player enters a new area
      cmapM_ $ \(Grid is) -> do
        let (floor -> gx) = (x1 / 150) + (signum x1)
            (floor -> gy) = (y1 / 150) + (signum y1)
            makeWaves r = do
              [a,b,c,d] <- liftIO $ (fmap.fmap) (\a ->  signum a * 70 + a) $ replicateM 4 (randomRIO (-100,100) :: IO Float)
              
              (Wall, BodyPicture p) <- head <$> getAll 
              wave (V2 1 1) ( r + V2 a b ) (abs $ 0.01 * V2 c d) p
            updateGrid g = cmap $ \(Grid _) -> Grid g
            moveStuff r e = do
              [o,p] <- liftIO $ fmap (\m -> signum m * 60 + m) <$> replicateM 2 (randomRIO (-20,20))
              e `set` Position (r + V2 o p)
              e `set` Velocity 0
        if length is > 10 then updateGrid mempty else return ()
        do 
          case M.lookup gx is of
            Just ys -> if gy `elem` M.keys ys then return ()
                       else replicateM 30 (makeWaves p2)
                            >> cmapM_ (\(Enemy,Position ep, e) -> when (norm (ep - p2) > 100) $ moveStuff p2 e)
                            >> updateGrid (M.insert gx (M.insert gy () ys) is) -- >> liftIO (print "new area")
            Nothing -> replicateM 30 (makeWaves p2)
                       >> cmapM_ (\(Enemy,Position ep, e) -> when (norm (ep - p2) > 100) $ moveStuff p2 e)
                       >> updateGrid (M.insert gx mempty is) 

      cmap $ goToPlayer p2
      cmapM_ $ hurtPlayer (Box b)


      --needs a seperate system for enemy bullets
      cmapM $ \case
        (Enemy,Charge c True,e) ->  if c < 1 then return (Charge (c + 0.005) True) else get e >>= shootPlayer >> return (Charge 0 True)
        (Enemy,Charge _ False,e) -> return (Charge (0.005) False)

      cmap checkProj
      
      Target tp@(V2 x2 y2) <- get global
      
      cmap $ \(Player) -> (Angle (vToRad $ p2 - tp))
      cmapM_ $ \(Player, _ :: Not Attacking) -> cmap hideSword
      cmapM_ $ \(Player, Attacking) -> do  
        cmapM_ $ \(Sword, Box sb) -> do
          cmap $ showSword x1 x2 tp p2
          cmapM_ $ killEnemy sb



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

