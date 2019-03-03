{-# LANGUAGE ViewPatterns #-}
module Ceph.Component.Player where

import Apecs
import Linear
import Control.Monad
import System.Random
import Ceph.Util
import Ceph.Physics.Box
import Ceph.Component.Projectile
import Ceph.Components
import Ceph.Component.Weapon
import Ceph.Component.Enemy
import Euterpea
import Graphics.Gloss
import qualified Data.IntMap as M

moveStuff :: V2 Float -> Entity -> Float -> System World ()
moveStuff r e a = do
  [o,p] <- liftIO $ fmap (\m -> signum m * a + m) <$> replicateM 2 (randomRIO (-1500,1500))
  e `modify` (\(Box (_,x,y)) -> Box ( (r + V2 o p) , x, y))
  e `set` (Velocity 0,Position (r + V2 o p))

playerLoop :: (BodyPicture, Player, Dash, Velocity, Box, Behavior, Charge, Entity) -> System World ()
playerLoop (BodyPicture bp,Player1, _, _, b@(Box (p1@(V2 x1 y1), _, _)), Attack, _, e) = cmapM_ $ \case
      (Sword, Box sb) -> do
        --e `set` BodyPicture (Pictures [bp, color (makeColor 0.1 0.1 0.1 0.01) $ ThickCircle 0.5 5])
          
        Target tp@(V2 x2 y2) <- get global
        cmap $ showSword x1 x2 tp p1
        cmap killEnemy
      _ -> return ()
playerLoop
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

playerLoop (_,Player1, Dash dx, Velocity v, b@(Box (p1@(V2 x1 y1),_,_)), a, Charge cv chging, e) = do
      
  cmap $ \(Target o) -> ( Target (o + v), Position (o + v), Velocity (o + v))
  cmapM_ $ \(Grid is) -> do
    let (floor -> gx) = (x1 / 2000) + (signum x1)
        (floor -> gy) = (y1 / 2000) + (signum y1)
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
      
player :: Picture -> Music Pitch -> System World Entity
player p m = newEntity (( Position (V2 0 50)
                        , 0 :: Velocity
                        , BodyPicture $ Scale (1/6) (1/6) p 
                        , Box (0, 1/3, 1/3))
                       , (ProjCount 30, Health 99, Dash 0)
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
               
