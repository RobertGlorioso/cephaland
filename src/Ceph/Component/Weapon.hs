{-# LANGUAGE FlexibleContexts #-}
module Ceph.Component.Weapon where

import Ceph.Components
import Ceph.Util

import Graphics.Gloss
import Apecs
import Linear


showSword ::
  Ord a =>
  a
  -> a
  -> V2 Float
  -> V2 Float
  -> (Weapon, Position, Angle, Velocity)
  -> (Weapon, Position, Angle, Velocity)
showSword x1 x2 tp pl (Sword,_,_,_) = (Sword
                                      , Position $ if x2 > x1
                                                   then pl + (V2 0.86 0.12)
                                                   else pl + (V2 (-0.86) 0.12)
                                      , Angle $ v2ToRad ( tp - pl )
                                      , Velocity 0
                                      )
showSword _ _ _ _ a = a

hideSword :: (Weapon, Position) -> (Weapon, Position)
hideSword (Sword,_) = (Sword, Position $ V2 (-100) (-1000) )
hideSword a = a

sword c = newEntity (BodyPicture (scale 0.1 0.1 c)
                    , Position (V2 (-1.05) 9.66)
                    , Velocity 0
                    , Angle 0
                    , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
                    , Sword) 

harpoon c = newEntity (BodyPicture (scale 0.6 0.6 c)
                    , Position (V2 (-10.05) 9.66)
                    , Velocity 0
                    , Angle 0
                    , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
                    , Harpoon) 
