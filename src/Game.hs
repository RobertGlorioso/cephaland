{-# LANGUAGE FlexibleContexts #-}

import Ceph.Scene
import Ceph.Components
import Ceph.Physics
import Ceph.Physics.Box
import Ceph.Entity.Enemy
import Ceph.Entity.Sword
import Ceph.Handler

import Apecs
import Options.Applicative
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import System.Random
import Linear

main :: IO ()
main = do
  --SDL.initialize [SDL.InitAudio]
  --M.openAudio M.defaultAudio 256
  w <- initWorld
  let opts = info (parseopts <**> helper)
        $ fullDesc
          <> progDesc "The Ceph Game"
          <> header "--debug turns on debugging, --connect <ipaddress>" 
  o <- liftIO (execParser opts)
  runSystem (initGame) w
  render o w
  stepper (1/60) w
  playIO (InWindow "seffyball" (640,480) (100,100)) (mixColors 0.1 0.9 white black) 60 w (render o) (\e w -> runSystem (handle e) w >> return w) stepper
  --M.closeAudio
  --M.quit
  --SDL.quit

parseopts = GameOpts <$> switch
  ( long "debug"
    <> help "Turn on in-game debug hud" )


initGame :: System World ()
initGame = do
  set global ( Camera 0 10
             , Gravity $ V2 0 (-0.009) )  
  replicateM 20 enemy 
  
  --read in sfx
--  sound <- liftIO $ (M.load "./resource/sfx/water.wav" :: IO M.Chunk)

  --read in sprites
  let handlePic :: Maybe Picture -> IO Picture
      handlePic = maybe
        (error "img not loading")
        return

  cig <- liftIO $ handlePic =<< loadJuicy "./resource/image/sword.png" 
  grund <- liftIO $ handlePic =<< loadJuicy "./resource/image/ground.png"
  arw <- liftIO $ handlePic =<< loadJuicy "./resource/image/arrow.png"
  octo <- liftIO $ handlePic =<< loadJuicy "./resource/image/octo1.png"
  sword cig
  
  let blck = do
        m <- randomRIO ((-500), (500) :: Int)
        y <- randomRIO ((-100), 0 :: Int)
        g <- randomRIO (1, 10 :: Int)
        a <- randomRIO (1, 10 :: Int)
        return $ [m,y,g,a]

  gblx <- liftIO $ (replicateM 500 blck)
  let bl [r,s,g,a] = let r' = fromIntegral r; s' = fromIntegral s; g' = fromIntegral g; a' = fromIntegral a; in (wall (V2 r' s') (V2 g' a'))
  mapM_ bl gblx
  
  newEntity (DynamicBody
            ,(Position (V2 0 50), Velocity (V2 0 0))
            , Resources [octo] [] --[sound]
            , BodyPicture $ scale 0.1 0.1 octo --(color blue $ rectangleSolid 1 1)
            , ProjCount 30
            , Box ((V2 0 0), 1, 1)
            , Player)
  newEntity (Target 0)
  newEntity (Dash 0)
  newEntity (Charge 0.01)
  return ()
