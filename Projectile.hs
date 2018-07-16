{-# LANGUAGE FlexibleContexts #-}
module Projectile where

import Apecs
import Apecs.Util
import Graphics.Gloss.Interface.IO.Game
import Linear
import Util
import Data 

removeProjectile otherBox (Projectile, Position p, Box pBox) =
  if aabb (Box otherBox) (Box pBox)
  then (Projectile, Position $ V2 (-100) (-100))
  else (Projectile, Position p)

shootArrow a x v =
  newEntity ( DynamicBody
            , ( Position x
              , Velocity $ v + normalize (a - x)
              , Angle 0
              )
            , ( BodyPicture . color orange $ Pictures [Line [(0,3), ( 0,0 )],  Line [ ( (-0.3), 0.3 ) , ( 0,0) ], Line [(0.3,0.3),(0,0)]]
              , Projectile
              , Box (x, 0.1, 0.07)
              )
            )
