{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ceph.Entity.Projectile where

import Ceph.Components
import Ceph.Util
import Ceph.Physics.Box

import Apecs
import Apecs.Util
import Graphics.Gloss.Interface.IO.Game
import System.CPUTime
import Linear

removeProjectile otherBox (Projectile, Position p, Box pBox, e) =
  if aabb (Box otherBox) (Box pBox)
  then e `destroy` (Proxy :: Proxy Position)
  else return ()

checkArrow (Velocity v, Angle t, Projectile) = (Projectile, Angle $ vToRad v + pi / 2) --if (norm v > 0.0001) then e `set` (Projectile, Angle $ vToRad v + pi / 2) else e `set` (Position $ pure 20000)


shootArrow a x v c = do
  newEntity ( Position x
            , Velocity $ v + (pure c) * normalize (a - x)
            , Angle 0
            , NoBehavior
            , ( BodyPicture . color orange $ Pictures [Line [(0,2), ( 0,0 )],  Line [ ( (-0.3), 0.3 ) , ( 0,0) ], Line [(0.3,0.3),(0,0)]]
              , Projectile
              , Box (x, 0.1, 0.07)
              )
            )
