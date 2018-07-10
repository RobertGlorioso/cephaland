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
            , ( BodyPicture . color orange $ rectangleSolid 0.4 0.4 
               -- scale 4 4 $ Pictures [Line [(0,3), ( 0,0 )],  Line [ ( (-0.3), 2.4 ) , ( 0,0.3 ) ], Line [(0.3,2.4),(0,3)]]
              , Projectile
              , Box (x, 0.1, 0.07)
              )
            )
