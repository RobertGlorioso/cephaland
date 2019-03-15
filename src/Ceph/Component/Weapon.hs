{-# LANGUAGE FlexibleContexts #-}
module Ceph.Component.Weapon where

import Ceph.Components
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
  
harpoon :: Picture -> System World Entity
harpoon c = newEntity (BodyPicture (scale 0.1 0.1 c)
                    , Position (V2 (-10.05) 9.66)
                    , Velocity 0
                    , Angle 0
                    , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
                    , Harpoon) 

chain :: Picture -> System World Entity
chain c = newEntity (BodyPicture (scale 0.06 0.06 c)
                    , Position (V2 (-10.05) 9.66)
                    , Velocity 0
                    , Angle 0
                    , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
                    , Chain)

chainExtended = do
  ls <- cfoldM (\a b -> return (b:a)) [] :: System World [(Linked, Position)]
  let (_, Position p1) = minimumBy (comparing fst) ls
  let (_, Position pn) = maximumBy (comparing fst) ls
  if norm ( p1 - pn ) > 50 then return (True,(p1,pn)) else return (False, (p1,pn)) 
