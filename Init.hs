{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Init where

import Control.Monad
import System.Random
import Linear
import Apecs
import Apecs.Util
import Graphics.Gloss
import Graphics.Gloss.Juicy
import qualified SDL.Mixer as M
import Util
import Enemy
import Data

blck = do
  m <- randomRIO ((-1000), 1000 :: Int)
  y <- randomRIO ((-10), 10 :: Int)
  g <- randomRIO (1, 10 :: Int)
  a <- randomRIO (1, 10 :: Int)
  return $ [m,y,g,a]

initGame :: System World ()
initGame = do
  set global ( Camera 0 10
             , Gravity $ V2 0 (-0.003) )  
  replicateM 20 enemy 
  
  --read in sfx
  sound <- liftIO $ (M.load "./resource/sfx/water.wav" :: IO M.Chunk)

  --read in sprites  
  cig <- liftIO $ handlePic =<< loadJuicy "./resource/image/sword.png" 
  grund <- liftIO $ handlePic =<< loadJuicy "./resource/image/ground.png"
  arw <- liftIO $ handlePic =<< loadJuicy "./resource/image/arrow.png"
  octo <- liftIO $ handlePic =<< loadJuicy "./resource/image/octo1.png"
  
  e1 <- newEntity(StaticBody
                 , (Position (V2 (-1.05) 9.66)
                   , Velocity 0
                   , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
                   )
                 , Sword)
  e1 `set` BodyPicture (scale 0.1 0.1 cig)
  
  gblx <- liftIO $ (replicateM 500 blck)
  let bl [r,s,g,a] = let r' = fromIntegral r; s' = fromIntegral s; g' = fromIntegral g; a' = fromIntegral a; in (boxy (V2 r' s') (V2 g' a'))
  mapM_ bl gblx
  
  newEntity (DynamicBody
            ,(Position (V2 0 50), Velocity (V2 0 0))
            , Resources [octo] [] --[sound]
            , BodyPicture $ scale 0.1 0.1 octo --(color blue $ rectangleSolid 1 1)
            , ProjCount 100
            , Box ((V2 0 0), 1, 1)
            , Player)
  newEntity (Target 0)
  newEntity (Dash 0)
  return ()
