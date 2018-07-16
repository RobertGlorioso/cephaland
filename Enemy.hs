{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}


module Enemy where

import Apecs
import Apecs.Util
import Control.Monad
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Linear
import qualified SDL.Mixer as M
import Util
import Data 

killEnemy painBox (Enemy, Position p, Box enemyBox) =
  if aabb (Box painBox) (Box enemyBox)
  then (Enemy, Position $ V2 (-100) (-100))
  else (Enemy, Position p)

enemy ::  System World ()
enemy = do
  g@(realToFrac -> g') <- liftIO $ randomRIO (0.1, 3 :: Float)
  [n,o,p,q] <- liftIO $ replicateM 4 $ randomRIO (-3, 3 :: Double)
  newEntity ( Enemy
              , ( DynamicBody
                , BodyPicture . color yellow $ Circle g
                , Velocity (V2 n o)
                , Position (V2 p q)
                , Angle 0
                )
              , ProjCount 10
              , Box ((V2 p q), g', g')
              )
  return ()
