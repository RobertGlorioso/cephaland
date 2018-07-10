module Sword where

import Apecs
import Apecs.Util
import Linear
import Data
import Util

showSword x1 x2 tp pl Sword = (Sword
                              , Position $ if x2 > x1
                                           then pl + (V2 0.36 0.12)
                                           else pl + (V2 (-0.36) 0.12)
                              , Angle $ vToRad ( tp - pl )
                              )
hideSword Sword = (Sword, Position $ V2 (-100) (-1000) )
