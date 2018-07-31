{-# LANGUAGE FlexibleContexts #-}
module Ceph.Entity.Projectile where

import Ceph.Components
import Ceph.Util
import Ceph.Physics.Box

import Apecs
import Apecs.Util
import Graphics.Gloss.Interface.IO.Game
import Linear

removeProjectile otherBox (Projectile, Position p, Box pBox, e) =
  if aabb (Box otherBox) (Box pBox)
  then e `destroy` (Proxy :: Proxy Position)
  else return()

shootArrow a x v c =
  newEntity ( DynamicBody
            , ( Position x
              , Velocity $ v + (pure c) * normalize (a - x)
              , Angle 0
              )
            , ( BodyPicture . color orange $ Pictures [Line [(0,2), ( 0,0 )],  Line [ ( (-0.3), 0.3 ) , ( 0,0) ], Line [(0.3,0.3),(0,0)]]
              , Projectile
              , Box (x, 0.1, 0.07)
              )
            )
