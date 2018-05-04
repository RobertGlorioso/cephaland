{-# LANGUAGE FlexibleContexts #-}
module Projectile where

import Apecs
import Apecs.Util
import Apecs.Physics
import Apecs.Physics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Linear
import Util
import Data 

removeProjectile playerBox (Projectile, Position p, Box pBox) =
  if aabb playerBox pBox
  then (Projectile, Position $ V2 (-100) (-100))
  else (Projectile, Position p)

arrowshape = cRectangle (V2 0.03 0.1)

shootArrow a x v =
  newEntity ( DynamicBody
            , ( Position x
              , Velocity $ v + 5*normalize (a - x)
              )
            , (Shape arrowshape
              , BodyPicture . color orange $
                Pictures [Line [( 0,0.1 ), ( 0,0 )],  Line [ ( (-0.01),0.08 ) , ( 0,0.1 ) ], Line [(0.01,0.08),(0,0.1)]]
              , Projectile
              , Box (x, 0.1, 0.07)
              )
            , a_material )
