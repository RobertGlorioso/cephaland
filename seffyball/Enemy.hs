{-# LANGUAGE FlexibleContexts #-}
module Enemy where

import Apecs
import Apecs.Util
import Apecs.Physics
import Apecs.Physics.Gloss
import Control.Monad
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Juicy
import qualified SDL.Mixer as M
import Util
import Data 

killEnemy painBox (Enemy, Position p, Box enemyBox) =
  if aabb painBox enemyBox
  then (Enemy, Position $ V2 (-100) (-100))
  else (Enemy, Position p)

enemy ::  System World ()
enemy = do
  g <- liftIO $ randomRIO (0.1, 0.3 :: Double)
  [n,o,p,q] <- liftIO $ replicateM 4 $ randomRIO (-30, 30 :: Double)
  newEntity ( Enemy
              , ( DynamicBody
                , BodyPicture . color yellow . toPicture $ cCircle g
                , Shape $ cCircle g
                , Velocity (V2 n o)
                , Position (V2 p q)
                )
              , ProjCount 10
              , Box ((V2 p q), g, g)
              , a_material)
  return ()
