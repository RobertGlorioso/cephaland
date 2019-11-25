{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Ceph.Scene
import Ceph.Util
import Ceph.Components
import Ceph.Physics
import Ceph.Physics.Box
import Ceph.Component.Enemy
import Ceph.Component.Wall
import Ceph.Component.Player
import Ceph.Component.Projectile
import Ceph.Component.Levels
import Ceph.Component.Weapon
import Ceph.Scene.Board
import Ceph.Handler
import Ceph.Jams
import Apecs
import Data.List (isInfixOf,isPrefixOf,zip4)
import Data.Bool
import Data.Time.Clock
import Options.Applicative
import Control.Monad
import Control.Concurrent
import System.Random
import System.Directory
import Data.Monoid
import Linear (V2(..))
import qualified SDL.Mixer as M 
import qualified SDL.Font as F --cannot find a font that works on windows
import qualified Data.Text as T
import qualified SDL as S
import SDL.Input
import Foreign.C.Types
import Euterpea

--todo:
--refactor init script to delegate to various components

main :: IO ()
main = do
  S.initialize [S.InitVideo, S.InitAudio]
  M.initialize []
  M.openAudio M.defaultAudio 256
  window <- S.createWindow "My SDL Application" S.defaultWindow
  S.showWindow window
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  w <- initWorld
  runSystem (initGame renderer) w
  
  --printing to stdout is not working on windows
  let opts = info (parseopts <**> helper)
        $ fullDesc
          <> progDesc "The Ceph Game"
          <> header "--debug turns on debugging logs" 
  o <- liftIO (execParser opts)
  
  mainLoop w renderer
  S.destroyRenderer renderer
  S.destroyWindow window
  S.quit
  where 
    initGame r = do
      beatsFiles <- liftIO $ getDirectoryContents "./resource/sfx"
                    >>= return . filter ((&&) <$> isInfixOf ".wav"  <*> ( flip elem ['A'..'Z'] . head ))
      instrumentFiles <- liftIO $ getDirectoryContents "./resource/sfx"
                    >>= return . filter ((&&) <$> isInfixOf ".wav"  <*> ( flip elem ['a'..'z'] . head ))
      bcs <- liftIO $ mapM (\f -> M.load $ "./resource/sfx/" ++ f) beatsFiles :: System World [M.Chunk]
      pfs <- liftIO $ mapM (\f -> M.load $ "./resource/sfx/" ++ f) instrumentFiles :: System World [M.Chunk]
      
      let enemyFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/squid1.bmp" 
      let chainFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/chain.bmp"
      let wallFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/wall.bmp"
      let octoFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/octo2.bmp"
      let bultFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/bullett.bmp"
      chnTexture <- liftIO $ loadTxtr r chainFile
      wallTexture <- liftIO $ loadTxtr r wallFile
      octoTexture <- liftIO $ loadTxtr r octoFile
      enmTexture <- liftIO $ loadTxtr r enemyFile
      bultTexture <- liftIO $ loadTxtr r bultFile
      targ <- newEntity (Target 0,Box (0,0,0),Angle 0,Position 0)
                
      walls <- liftIO $
        zip4 <$>
          randomDonutBox 100 600 900
          <*> randomDonutBox 100 600 400
          <*> replicateM 40 (randomRIO (20,50 :: CDouble))
          <*> replicateM 100 (randomRIO (20,50 :: CDouble))
      wallEs <- mapM (\(w, b) -> hardWall w b wallTexture) $ zip walls ( concat . repeat $ zip bcs pfs )
    
      mapM_ (newBullet bultTexture) $ zip bcs pfs
                  
      mapM_ (enemy enmTexture) $ zip bcs pfs
      
      pl <- player octoTexture (head pfs) (head bcs)

      let makeChains i = replicateM i (chain chnTexture) >>= (\chns -> chains (pl:chns) targ)
      
      mapM_ makeChains [10..20]
      
      cmap (\bx -> bool Out In $ aabb bx (Box (0, 600, 600)))
      set global (Camera 0 1.0
                  , SDLRenderer r
                  , Beat 30 0
                  , listToBoard (wallEs) :: Sequencer
                  , mempty :: SCoord
                  , BoardControl Play Unlocked
                  )
      newEntity ( Grid mempty )
      randomizeGridCell 0  


mainLoop :: World -> S.Renderer -> IO ()
mainLoop w renderer = do
  events <- S.pollEvents
  t1 <- getCurrentTime
  let quit = elem S.QuitEvent $ map S.eventPayload events
  S.clear renderer
  S.rendererDrawColor renderer S.$= S.V4 190 190 190 255
  render w
  S.present renderer
  physicsStep w
  t2 <- getCurrentTime
  let d = round $ (100000) * (diffUTCTime t2 t1)
  when (d < 16000) $ threadDelay (16000 - d)
  unless (quit) (mainLoop w renderer )

parseopts = GameOpts <$> switch
  ( long "debug"
    <> help "Turn on in-game debug hud" )