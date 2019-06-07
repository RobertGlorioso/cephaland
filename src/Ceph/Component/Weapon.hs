{-# LANGUAGE FlexibleContexts #-}
module Ceph.Component.Weapon where

import Ceph.Components
import Ceph.Physics.Box
import Ceph.Util

import Graphics.Gloss
import Apecs
import Linear
import Data.List
import Data.Ord

showSword ::
  Ord a =>
  a
  -> a
  -> V2 Float
  -> V2 Float
  -> (Weapon, Position, Angle, Velocity)
  -> (Weapon, Position, Angle, Velocity)
showSword x1 x2 tp pl (Sword,_,_,_) =
  (Sword
  , Position $ if x2 > x1
               then pl + (V2 0.86 0.12)
               else pl + (V2 (-0.86) 0.12)
  , Angle $ v2ToRad ( tp - pl )
  , Velocity 0
  )
showSword _ _ _ _ a = a

hideSword :: (Weapon, Position) -> (Weapon, Position)
hideSword (Sword,_) = (Sword, Position $ pure 2e7 )
hideSword a = a

sword :: Picture -> System World Entity
sword c = newEntity (BodyPicture (scale 0.1 0.1 c)
                    , Position (V2 (-1.05) 9.66)
                    , Velocity 0
                    , Angle 0
                    , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
                    , Sword)

laser :: Picture -> System World Entity
laser c = newEntity (BodyPicture (scale 0.1 0.1 c)
                    , Position (pure 2e7)
                    , Velocity 0
                    , Angle 0
                    , Box ((pure 2e7), 0.04, 0.04)
                    , Laser) 
           
harpoon :: Picture -> System World Entity
harpoon c = newEntity (BodyPicture (scale 0.1 0.1 c)
                    , Position (V2 (-10.05) 9.66)
                    , Velocity 0
                    , Angle 0
                    , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
                    , Harpoon)
  
chain :: Picture -> System World Entity
chain pic = newEntity (Chain
                     , Weapon
                     , NoBehavior
                     , Angle 0
                     , Position 0
                     , Velocity 0
                     , ( box 0 0.05 0.05
                       , BodyPicture $ Pictures
                         [Line [(0,0), (10,0)]
                         ,Scale (0.03) (0.03) pic]
                       )
                     )
  
chains :: [Entity] -> Entity -> System World ()
chains [] targ = return ()
chains (_:lst:[]) targ = lst `set` (WLinked lst targ 1.0 )
chains (prev:cur:nxt:rst) t = do
  cur `set` (Linked prev nxt)
  chains (cur:nxt:rst) t
  
chainExtended ::
  Float -> System World (Bool, (V2 Float, V2 Float))
chainExtended r = do
  ls <- cfoldM (\a b -> return (b:a)) [] :: System World [(Linked, Position)]
  let (_, Position p1) = minimumBy (comparing fst) ls
  let (_, Position pn) = maximumBy (comparing fst) ls
  if norm ( p1 - pn ) > r then return (True,(p1,pn)) else return (False, (p1,pn)) 

moveChains :: (Linked, Weapon) -> System World (Angle, Position)
moveChains (Linked e f, Chain) = do
          (Position p1) <- get e
          (Position p0) <- get f
          return $ (Angle $ v2ToRad (p0 - p1), Position $ (p0 + p1) / 2)
moveChains (WLinked e f m, Chain) = do
          (Position p0) <- get e
          (Position p1) <- get f
          return $ (Angle $ v2ToRad (p0 - p1), Position $ (pure (1-m) * p0 + (pure m * p1)))
