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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}


module Ceph.Physics where

import Ceph.Util
import Ceph.Jams
import Ceph.Physics.Box
import Ceph.Components
import Ceph.Component.Projectile
import Ceph.Component.Weapon
import Ceph.Component.Enemy

import Apecs
import Apecs.System
import Control.Monad
import Data.Bool
import qualified Data.IntMap as M
import System.Random
import Linear
import Euterpea
import Graphics.Gloss

moveStuff :: V2 Float -> Entity -> Float -> System World ()
moveStuff r e a = do
  [o,p] <- liftIO $ fmap (\m -> signum m * a + m) <$> replicateM 2 (randomRIO (-1000,1000))
  e `modify` (\(Box (_,x,y)) -> Box ( (r + V2 o p) , x, y))
  e `set` (Velocity 0,Position (r + V2 o p))
          
stepper :: Float -> World -> IO World
stepper _ !w = runWith w $ do
  incrementBeat w
  [(Player1, Position p)] <- getAll
  cmapM playerLoop1
  enemyLoop p
  motion
  return w
  where
    incrementBeat w = do
      Beat m i <- get global
      -- plays sound effects on beat
      -- maybe do other stuff on beat like animations?
      if (m == i) then (global `set` Beat m 0) >> cmapM_ ( \case
        (Sing, Song i, a :: Actor, e) ->  e `set` (Debug . show $ (a, Song i)) >> playSong w e >> when ( a == Weapon ) ( e `set` Seek )
        _ -> return () 
        )
        else global `set` Beat m (i+1) 
      liftIO . print =<< flip cfoldM (Song (rest 0)) (\s@(Song i) ->
                (\case
                    (Sing, Song j) -> return (Song $ i :=: j)
                    _ -> return (Song i)
                )
              )
                                  
    motion = do
      Gravity g <- get global
      --moves all objects that are in motion (have a Velocity and Box )
      --forkSys . atomically .
      cmap $ \case
        c@( _, _, _, Plant, _) -> c
        (Box (_,w,h), Position p, Velocity v, s, Weapon) -> (Box (p+v,w,h), Position $ p + v, Velocity v, s, Weapon)
        (Box (_,w,h), Position p, Velocity v, Seek, f) -> (Box (p+v,w,h), Position $ p + v, Velocity v, Seek, f)
        (Box (_,w,h), Position p, Velocity v, e, f) -> (Box (p+v,w,h), Position $ p + v, Velocity $ ( v + g ) * 0.999 , e, f)

      --correct angle for certain moving objs
      cmap $ \(Velocity v, Angle t, a ) -> if a == Enemy || a == Projectile then (Angle $ v2ToRad v) else Angle t

      cmapM $ \(Linked e f, Chain) -> do
        [x,y] <- liftIO $ replicateM 2 (randomRIO (0.1,1) :: IO Float)
        (Position p1) <- get e
        (Position p0) <- get f
        return $ (Angle $ v2ToRad (p0 - p1), Position $ (p0 + p1) / 2)

      --updates scope for rendering & collision detection
      view@(Camera cam scale) <- get global :: System World Camera
      cmap (\b -> bool Out In $ aabb b (Box (cam, 400, 400)))

      --physics for colliding with walls
      boxBound
      
    playerLoop1 :: (BodyPicture, Player, Dash, Velocity, Box, Behavior, Charge, Entity) -> System World ()
    playerLoop1 (BodyPicture bp,Player1, _, _, b@(Box (p1@(V2 x1 y1), _, _)), Attack, _, e) = cmapM_ $ \case
      (Sword, Box sb) -> do
        e `set` BodyPicture (Pictures [bp, color (makeColor 0.1 0.1 0.1 0.01) $ ThickCircle 0.5 5])
          
        Target tp@(V2 x2 y2) <- get global
        cmap $ showSword x1 x2 tp p1
        cmap killEnemy
      _ -> return ()
    playerLoop1
      (_, Player1, _, _, b@(Box (p1,_,_)), Carry, _, _) =
      conceIf
        (\(p, a) -> aabb b p && a == Wall)
        (\(Box (_,x,y)) -> (Box (p1, x, y), Position p1))
      --allows the player to pick up an enemy or other entity
      
      --e <- return . filter (\(p, _, a) -> aabb b p && a == Wall) =<< (getAll :: System World [(Box, Entity, Actor)])
      --if length e == 0 then return () else do
        --let (_,carriedEnt,_) = head e
        --carriedEnt `modify` (\(Box (_,x,y)) -> Box (p1, x, y))
        --carriedEnt `set` (Position p1)

    playerLoop1 (_,Player1, Dash dx, Velocity v, b@(Box (p1@(V2 x1 y1),_,_)), a, Charge cv chging, e) = do
      
      cmap $ \(Target o) -> ( Target (o + v), Position (o + v), Velocity (o + v))
      cmapM_ $ \(Grid is) -> do
        let (floor -> gx) = (x1 / 500) + (signum x1)
            (floor -> gy) = (y1 / 500) + (signum y1)
            updateGrid g = cmap $ \(Grid _) -> Grid g
            moveEnemyWalls =
              cmapM_ $ \case
                (Wall, Out, e) -> moveStuff p1 e 200
                (Enemy, Out, e) -> moveStuff p1 e 200
                (_,_,_) -> return ()
                                    
        if length is > 10 then updateGrid mempty else return ()
        case M.lookup gx is of
          Just ys -> if gy `elem` M.keys ys then return ()
                     else updateGrid (M.insert gx (M.insert gy () ys) is) >> moveEnemyWalls
          Nothing -> moveEnemyWalls >> updateGrid (M.insert gx mempty is)
      Target tp <- get global
      
      cmapM_ $ \case
        (Weapon, In, b) -> cmap (hurtEnemy b)
        (_, _, _) -> return () 

      let chg
            | chging = if cv < 10 then (Charge (cv + 0.1) True) else (Charge cv False)
            | True   = Charge cv False
      let dsh
            | dx < 8.0 = Dash (dx + 0.3)
            | True     = Dash dx

      [nv,nvv] <- liftIO $ replicateM 2 $ randomRIO (-0.05, 0.05)        
      let newV = if ( a /= Seek ) then v else v + V2 nv nvv                                                             
      e `set` (Player, Angle (v2ToRad $ p1 - tp), chg, dsh, Velocity newV)    
        



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

