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
  replicateM 100 enemy 
  
  --read in sfx
  sound <- liftIO $ (M.load "./resource/sfx/water.wav")

  --read in sprites  
  cig <- liftIO $ handlePic =<< loadJuicy "./resource/image/sword.png" 
  grund <- liftIO $ handlePic =<< loadJuicy "./resource/image/ground.png"
  arw <- liftIO $ handlePic =<< loadJuicy "./resource/image/arrow.png"
  octo <- liftIO $ handlePic =<< loadJuicy "./resource/image/octo1.png"
  
  e1 <- newEntity(StaticBody
                 , (Position (V2 (-1.05) 9.66)
                   , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
                   )
                 , Sword)
  e1 `set` BodyPicture (scale 0.1 0.1 cig)
  
  let boxy (V2 c1@(realToFrac -> c1' :: Double) c2@(realToFrac -> c2')) (V2 w@(realToFrac -> w' :: Double) h@(realToFrac -> h')) = newEntity (StaticBody, Wall, Angle 0, Position (V2 c1' c2'), Box (V2 c1' c2', w', h'), BodyPicture $ color red $ rectangleSolid (2*w) (2*h) )
           
  gblx <- liftIO $ (replicateM 500 blck)
  let bl [r,s,g,a] = let r' = fromIntegral r; s' = fromIntegral s; g' = fromIntegral g; a' = fromIntegral a; in (boxy (V2 r' s') (V2 g' a'))
  mapM_ bl gblx
  {--gridLines'' (V2 3 0) (V2 1 8) 10 10
  gridLines'' (V2 30 10) (V2 1 8) 1 10
  gridLines'' (V2 330 0) (V2 1 8) 50 100
--}
  newEntity (DynamicBody
            ,(Position (V2 0 50), Velocity (V2 0 0))
            , Resources [color blue $ rectangleSolid 1 1] [sound]-- [octo] [sound]
            , BodyPicture (color blue $ rectangleSolid 1 1)
            , ProjCount 100
            , Box ((V2 0 0), 1, 1)
            , Player)
  newEntity (Target 0)
  newEntity (Dash 0)
  return ()
