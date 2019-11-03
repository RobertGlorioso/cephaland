{-# LANGUAGE ViewPatterns #-}
module Ceph.Component.Player where

  
import Ceph.Util
import Ceph.Physics.Box
import Ceph.Component.Projectile
import Ceph.Components
import Ceph.Component.Weapon
import Ceph.Component.Enemy

import Apecs
import Linear
import Data.List
import Data.Ord
import Control.Monad
import System.Random
import Foreign.C.Types
import qualified SDL as S

playerLoop :: (Player, Dash, Velocity, Box, Behavior, Charge, Entity) -> System World ()
playerLoop (Player1, _, v0@(Velocity vv@(V2 vx _)) , _, Moving v2, _, e) = do
  cmap $ \(Target o) -> ( Target (o + vv), Position (o + vv))
  set e $ if (norm vv < speedLimit) then (v0 + Velocity v2) else v0
  cmapM_ $ \case
    (b,Bullet) -> cmap (hurtPlayer b)
    _ -> return ()
    
playerLoop (Player1, _, _, _, Swinging, _, e) = do
  (chex,(p1,pn)) <- chainExtended 30
  (chex2,(p1,pn)) <- chainExtended 50
  if chex then
    e `modify`
      (\(Velocity v,_) ->
        (Velocity $ v + ( (if chex2 then 0.02 else 0.002) * normalize  (pn - p1))
        , Swinging))
    else return ()
playerLoop (Player1, _, _, b@(Box (p1@(V2 x1 y1), _, _)), Attack, _, e) =
  cmapM_ $ \case
    (Sword, Box sb) -> do
      Target tp@(V2 x2 y2) <- get global
      cmap $ showSword x1 x2 tp p1
    _ -> return ()
playerLoop
  (Player1, _, _, b@(Box (p1,_,_)), Carry, _, _) =
  conceIf
    (\(p, a) -> aabb b p && a == Wall)
    (\(Box (_,x,y)) -> (Box (p1, x, y), Position p1))
  --allows the player to pick up an enemy or other entity
      
      --e <- return . filter (\(p, _, a) -> aabb b p && a == Wall) =<< (getAll :: System World [(Box, Entity, Actor)])
      --if length e == 0 then return () else do
        --let (_,carriedEnt,_) = head e
        --carriedEnt `modify` (\(Box (_,x,y)) -> Box (p1, x, y))
        --carriedEnt `set` (Position p1)

playerLoop (Player1, Dash dx, Velocity v, b@(Box (p1,_,_)), a, Charge cv chging, e) = do
      
  cmap $ \(Target o) -> ( Target (o + v), Position (o + v))
  cmapM_ $ \case
    (b,Bullet) -> cmap (hurtPlayer b)
    _ -> return ()
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
                                       
  e `set` (Player, Angle (v2ToRad $ p1 - tp), chg, dsh, Velocity v )     


player :: Txtr -> System World Entity
player txtr = newEntity (( Position (V2 0 50)
                        , 0 :: Velocity
                        , Gravity (V2 0 (0.1))
                        , txtr
                        , Box (0, 1, 1))
                       , (ProjCount 30, Health 99, Dash 0)
                       , (Player1, Player)
                       , (NoBehavior, Charge 0.01 False, SFXResources [] []))

playerShootArrow :: (Charge, Position, Velocity, ProjCount, Player, Entity) -> System World ()
playerShootArrow o@(c, x, v, ProjCount arrowsLeft, Player1, e) = do
        t <- get global 
        when (arrowsLeft >= 1) $ do
          shootArrow t x v c
          e `set` (Charge 1.0 False, ProjCount $ arrowsLeft - 1)

playerShootChain :: (Charge, Position, Velocity, Player) -> System World (Charge, Behavior)
playerShootChain o@(c, x, v, Player1) = do
        t <- get global 
        shootChains t x v c
        return (Charge 1.0 False,Swinging)

speedLimit :: CDouble
speedLimit = 6

movePlayer :: V2 CDouble -> (Player, Velocity, Behavior) -> (Player, Velocity, Behavior)
movePlayer v c@(d, p, Swinging) = (d, p + Velocity (10*v), Swinging )  
movePlayer v c@(d, Velocity p, b)
  | norm p > speedLimit = (d, Velocity p,b)
  | True = (d, Velocity p, Moving v)

playerSwinging :: System World ()
playerSwinging = do
  ls <- cfoldM (\a b -> return (b:a)) [] :: System World [(Linked, (Position, Entity))]
  tt <- cfoldM (\a b -> return (b:a)) []
  let [(Target t)] = tt 
  conceIfM_
    (\case
        (_, Out, _, _) -> False
        (Wall1, In, n, c) -> let new_b = (snd $ rotate_box_cw c (Position t,Box (t, 0.5, 0.5) ) n) in touched new_b c
    )
    (\(Wall1) -> do
        cmap $ \(Player1) -> Swinging --return Swinging 
    )
    
playerDash :: Target -> (Player, Position, Velocity, Dash) -> (Player, Position, Velocity, Dash)
playerDash (Target o) pp@(Player1, Position p,_, Dash w) = do
  if w >= 2.0
  then (Player1, Position p, Velocity $ normalize (o - p), Dash (-2.0))
  else pp      

hurtPlayer  :: (Box,Projectile) -> (Box, Health, Player) -> Health
hurtPlayer ((Box painBox),Bullet) (Box plBox, h, _)  = if (aabb (Box painBox) (Box plBox)) then (h - 10) else h
hurtPlayer (_,_) (_, h, _)  = h
