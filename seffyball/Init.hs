{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Init where

import Control.Monad
import System.Random
import Apecs
import Apecs.Util
import Apecs.Physics
import Apecs.Physics.Gloss
import Graphics.Gloss
import Graphics.Gloss.Juicy
import qualified SDL.Mixer as M
import Util
import Enemy
import Data

blck = do
  m <- randomRIO (1, 100 :: Int)
  y <- randomRIO (1, 100 :: Int)
  g <- randomRIO (1, 10 :: Int)
  a <- randomRIO (1, 10 :: Int)
  m <- randomRIO (1, 10 :: Int)
  n <- randomRIO (1, 10 :: Int)
  return [m,y,g,a,m,n]

initGame :: System World ()
initGame = do
  setGlobal ( Camera 0 50
            , Gravity $ V2 0 (-6.3) )  
  replicateM 100 enemy 
  let ballshape = cCircle 0.1
      ball = ( DynamicBody
             , Shape ballshape
             , BodyPicture . color chartreuse . toPicture $ ballshape
             , a_material )
      sballshape = cCircle 0.01

  --read in sfx
  sound <- liftIO $ (M.load "./resource/water.wav")

  --read in sprites  
  cig <- liftIO $ handlePic =<< loadJuicy "./resource/sword.png" 
  grund <- liftIO $ handlePic =<< loadJuicy "./resource/ground.png"
  arw <- liftIO $ handlePic =<< loadJuicy "./resource/arrow.png"            
  --add a sword to hit stuff
  e1 <- newEntity(StaticBody
                 , (Position (V2 (-1.05) 9.66)
                   , Shape $ cRectangle (V2 0.07 0.42)
                   , a_material
                   , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
                   )
                 , Sword)
  e1 `set` BodyPicture (scale 0.01 0.01 cig)
  
  let --gridLines'' :: (V2 Float) -> (V2 Float) -> Int -> Int -> System World (Entity e)
      gridLines'' center size c r = do
        grid <- newEntity (StaticBody
                          , Wall
                          , a_material
                          , Position (V2 c1' c2')
                          , Box (V2 c1' c2', 0.25*w', 0.25*h')
                          , BodyPicture $ translate c1 c2 $ scale (0.017*w) (0.0115*h) grund)
        forM verrts $ \l -> newEntity ( ShapeExtend (cast grid) (setRadius 0.05 l))
        return grid
                  
        where
          verrts = [ Apecs.Physics.shift (V2 (x+c1') c2') (vLine $ h' - 0.05) | x <- xs ] ++ [ Apecs.Physics.shift (V2 c1' (y+c2')) (hLine w') | y <- ys ]
          V2 c1@(realToFrac -> c1' :: Double) c2@(realToFrac -> c2') = center
          V2 w@(realToFrac -> w' :: Double) h@(realToFrac -> h') = size
          dx = w'/fromIntegral c
          dy = h'/fromIntegral r
          xs = [-w'*0.5 + fromIntegral n * dx | n <- [0..c]]
          ys = [-h'*0.5 + fromIntegral n * dy | n <- [0..r]]

  gridLines'' (V2 3 3) (V2 18 8) 10 5
  gblx <- liftIO $ (replicateM 50 blck)
  let bl [r,s,g,a,m,n] = let r' = fromIntegral r; s' = fromIntegral s; g' = fromIntegral g; a' = fromIntegral a; in (gridLines'' (V2 r' s') (V2 g' a') m n)
  mapM_ bl gblx


  octo <- liftIO $ handlePic =<< loadJuicy "./resource/octo.png"      
  newEntity ((DynamicBody, Shape $ cCircle 0.12, a_material)
            ,(Position (V2 11.05 6.33), Velocity (V2 (-10) 10))
            , Resources [octo] [sound]
            , ProjCount 10
            , Box ((V2 (13.05) 6.33), 0.1, 0.1)
            , Player)
  newEntity (Target 0)
  newEntity (Dash 0)
  return ()
