{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Ceph.Entity.Enemy where

import Ceph.Util
import Ceph.Components
import Ceph.Physics.Box

import Apecs
import Apecs.Util
import Control.Monad
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Linear

killEnemy painBox (Enemy, Box enemyBox, e) =
  if aabb (Box painBox) (Box enemyBox)
  then destroy e  (Proxy :: Proxy Position) --(Enemy, Position $ V2 (-100) (-100))
  else return ()

goToPlayer :: (Has World Enemy) => V2 Double -> Entity -> (Enemy, Velocity, Position, Entity) -> System World ()
goToPlayer m (Entity e') (Enemy, Velocity v@(V2 e1 e2), Position p, Entity e) = do
  --liftIO $ print (e, e')
  if (e == e') then (Entity e) `set` (Enemy, Velocity $ (pure $ 0.1 + norm v) * (normalize (m - p))) else return () 

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
              , ProjCount 3
              , Box ((V2 p q), g', g')
              )
  return ()
