{-# LANGUAGE FlexibleContexts #-}
module Ceph.Component.Sword where

import Ceph.Components
import Ceph.Util

import Graphics.Gloss
import Apecs
import Linear

showSword x1 x2 tp pl Sword = (Sword
                              , Position $ if x2 > x1
                                           then pl + (V2 0.86 0.12)
                                           else pl + (V2 (-0.86) 0.12)
                              , Angle $ v2ToRad ( tp - pl )
                              , Velocity 0
                              )
hideSword Sword = (Sword, Position $ V2 (-100) (-1000) )

sword c = newEntity (BodyPicture (scale 0.1 0.1 c)
                    , Position (V2 (-1.05) 9.66)
                    , Velocity 0
                    , Angle 0
                    , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
                    , Sword) 
