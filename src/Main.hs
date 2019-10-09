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
import Ceph.Component.Player
import Ceph.Component.Projectile
import Ceph.Component.Levels
import Ceph.Component.Weapon
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
--import qualified SDL.Image as I
import qualified SDL.Font as F
import qualified Data.Text as T
import qualified SDL as S
import SDL.Input
import Foreign.C.Types
import Euterpea

main' :: IO ()
main' = do
  t1 <- getCurrentTime
  threadDelay 10000000
  t2 <- getCurrentTime
  writeFile "time.txt" (show . round $ 100000 * diffUTCTime t2 t1)

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
          pianosFiles <- liftIO $ getDirectoryContents "./resource/sfx"
                        >>= return . filter ((&&) <$> isInfixOf ".wav"  <*> ( flip elem ['a'..'z'] . head ))
          bcs <- liftIO $ mapM (\f -> M.load $ "./resource/sfx/" ++ f) beatsFiles :: System World [M.Chunk]
          pfs <- liftIO $ mapM (\f -> M.load $ "./resource/sfx/" ++ f) pianosFiles :: System World [M.Chunk]
          set global ( Camera 0 1.0
                , SDLRenderer r
                , Beat 30 0
                , mempty :: Sequencer
                , mempty :: SCoord
                )
          let enemyFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/squid1.bmp" 
          let chainFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/chain2.bmp"
          let wallFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/coral1.bmp"
          let octoFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/octo2.bmp"
          
          chnTexture <- liftIO $ loadTxtr r chainFile
          wallTexture <- liftIO $ loadTxtr r wallFile
          octoTexture <- liftIO $ loadTxtr r octoFile
          enmTexture <- liftIO $ loadTxtr r enemyFile
          targ <- newEntity (Target 0,Box (0,0,0),Angle 0,Position 0)
          let bl (r,s,g,a) (d,o) = do
                    newEntity ((Wall,Wall1)
                              , Position (V2 r s)
                              , Angle ( if g > a then 2*pi - (g + a) / 10 else (g + a) / 10 )
                              , Velocity 0
                              , box (V2 r s) (g) (a)
                              , SFXResources [d,o] []
                              , BodyPicture $ wallTexture -- (\(Txtr t size) -> Txtr t (S.Rectangle 0 $ S.V2 (round $ 2 * g) (round $ 2 * a))) wallTexture
                              ) 
                    
          walls <- liftIO $
            zip4 <$>
              randomDonutBox 200 600 900
              <*> randomDonutBox 200 600 400
              <*> replicateM 100 (randomRIO (20,50 :: CDouble))
              <*> replicateM 100 (randomRIO (20,50 :: CDouble))
          mapM_ (\(w, b) -> bl w b) $ zip walls (zip bcs pfs )
            
          pl <- player octoTexture
          chns <- replicateM 10 (chain chnTexture)
          chains (pl:chns) targ
          
          --mapM_ (newArrow (polygon [(0,0),(1,0),(1,2)])) $ concat . replicate 10 $ zip am cm
          --mapM_ (newBullet bults) $ concat . replicate 10 $ zip am cm
                      
          --replicateM_ 10 $ enemy enmTexture
          
          newEntity ( Grid mempty )
          
          cmap (\bx -> bool Out In $ aabb bx (Box (0, 600, 600)))
    
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