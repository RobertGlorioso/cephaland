{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Ceph.Scene
import Ceph.Scene.Board
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
import Ceph.Jams

import Apecs
import Data.List (isInfixOf,zip4,find)
import Data.Maybe
import Data.Bool
import Data.Time.Clock
import Options.Applicative
import Control.Monad
import Control.Concurrent
import System.Random
import System.Directory
import Linear (V2(..))
import qualified SDL.Mixer as M 
import qualified SDL as S
import Foreign.C.Types
import Euterpea
import Data.Monoid


main :: IO ()
main = do
  S.initialize [S.InitVideo, S.InitAudio]
  M.initialize []
  M.openAudio M.defaultAudio 256
  M.setChannels 128
  w <- initWorld
  window <- S.createWindow "CEPH" S.defaultWindow
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  runSystem (initGame renderer) w
  S.showWindow window
  S.HintRenderScaleQuality S.$= S.ScaleLinear
  {--
  let opts = info (parseopts <**> helper)
        $ fullDesc
          <> progDesc "The Ceph Game"
          <> header "--debug turns on debugging logs" 
  o <- liftIO (execParser opts) --}
  
  mainLoop w renderer
  S.destroyRenderer renderer
  S.destroyWindow window
  M.quit
  S.quit

initGame :: S.Renderer -> System World ()
initGame r = do
  let mkSFX ms col = do
        sfx <- liftIO $ mapM midiLoad ms
        return $ getZipList $ SFXResources <$> ZipList sfx <*> ZipList ms <*> ZipList (makeColorsMidi col)
  
  asmplSfxs <- liftIO $ mkSFX (makeMidiSamples Flute) (S.V4 50 0 50 0)
  beatSfxs <- liftIO $ mkSFX (makeMidiSamples Percussion) (S.V4 0 50 50 0)
  csmplSfxs <- liftIO $ mkSFX (makeMidiSamples Oboe) (S.V4 50 50 0 0)

  let enemyFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/squid1.bmp" 
  --let chainFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/chain.bmp"
  let wallFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/wall.bmp"
  let wallFile2 = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/wall1.bmp"
  let wallFile3 = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/wall2.bmp"
  let floorFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/coral1.bmp"
  let octoFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/octo2.bmp"
  let bultFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/bullett.bmp"
  let netFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/starshell.bmp"
  --chnTexture <- liftIO $ loadTxtr r chainFile
  flrTexture <- liftIO $ loadTxtr r floorFile
  octoTexture <- liftIO $ loadTxtr r octoFile
  bultTexture <- liftIO $ loadTxtr r bultFile
  netTexture <- liftIO $ loadTxtr r netFile
  wallTexture2 <- liftIO $ loadTxtr r wallFile2

  
  --let squalls v = newSquall (Position v) bultTexture (beatSfxs !! 1)
  --sqx <- liftIO $ replicateM 10 $ randomRIO (-100,100) :: System World [CDouble]
  --sqy <- liftIO $ replicateM 10 $ randomRIO (-100,100) 
  --sqs <- mapM squalls $ zipWith (V2) sqx sqy
  newSquall (Position $ pure 30) netTexture (beatSfxs !! 1) -- >>= \e -> set e $ Netted sqs
  tg <- newEntity (Target $ pure 10, Box (0,0,0), Position 0)

  let wallbxs r s = zip [(x-z,r+y,0,0) | (z,y) <- zip [0,12,24,36] [-350,-300..350], x <- [-200,-177..50]] s
  let makeWalls wall wp = mapM (\(w,bx) -> nonWall w bx wallTexture2) $ concat (getZipList $ wp <$> ZipList [0,200,400] <*> ZipList [beatSfxs, asmplSfxs, csmplSfxs])
  
  let circleOfFifths o = take 3 $ iterate (\j -> drop 3 j ++ take 3 j) $ fmap (\i -> find ((==i) . getPC . song) asmplSfxs) ((Note qn . (,o)) <$> [F,C,G,D,A,E,B,Fs,Cs,Af,Ef,Bf])
  let tonnetzBxs r o = zip [(x-a,r+y,0,0) | (a,y) <- zip [0,12..100] [-250,-225..0], x <- [200,225..450]] (catMaybes . concat $ circleOfFifths o)
  
  wallEs <- makeWalls Wall1 wallbxs
  tonnetzWalls <- mapM (\(w,bx) -> nonWall w bx wallTexture2) $ concat [tonnetzBxs 200 3,tonnetzBxs 0 4]
  
  pl <- player1 octoTexture (last beatSfxs)
  oneWayWall (-600, -40) (head $ asmplSfxs) flrTexture
  oneWayWall (600, -110) (head $ asmplSfxs) flrTexture
  
  wallboxs <- mapM get $ wallEs ++ tonnetzWalls ++ [pl] :: System World [Box]      
  let nr = MBoard ( zip wallboxs (wallEs ++ tonnetzWalls ++ [pl]) ) True :: Netitor
  nt <- net netTexture nr
  wChains 0.5 (pl:nt:tg:[]) tg
  
  mapM_ (newBullet bultTexture) beatSfxs
              
  enms1 <- mapM (enemy r enemyFile Enemy1) =<< liftIO (replicateM 1 $ takeRandom beatSfxs)
  enms2 <- mapM (enemy r enemyFile Enemy1) =<< liftIO (replicateM 1 $ takeRandom asmplSfxs)
  enms3 <- mapM (enemy r enemyFile Enemy1) =<< liftIO (replicateM 1 $ takeRandom csmplSfxs)

  makeFloorWallBox flrTexture (pure 0) beatSfxs
  --makeFloorWallBox flrTexture (V2 (-1200) 0) asmplSfxs
  --makeFloorWallBox flrTexture (V2 0 1200) csmplSfxs

  let makeChains i = replicateM i (chain' (head beatSfxs)) >>= (\chns -> chains (pl:chns) tg)
  
  mapM_ makeChains [10..20] 

  let makeSequencer :: [SFXResources] -> System World ()
      makeSequencer sfxs = do
        rest <- return $ last sfxs
        restEnt <- newEntity rest
        _ <- newEntity (getInst $ song rest, listToBoard (repeat [restEnt]) :: Sequencer)
        return ()

  makeSequencer asmplSfxs
  makeSequencer csmplSfxs
  makeSequencer beatSfxs
  _ <- newEntity( musicClock, Position $ V2 360 60 )

  cmap (\bx -> bool Out In $ aabb bx (Box (0, 600, 600)))
  cmap $ \(SFXResources _ s _ ) -> getInst s
  set global (Camera 0 1.0
              , SDLRenderer r
              , Beat 40 0
              , mempty :: SCoord
              , mempty :: SongList
              , MCoordF (Box (0,1,1)) (Entity 10)
              , BoardControl Play Unlocked 0 [S1,S2,S3,S4]
              )
  _ <- newEntity ( Grid mempty )
  randomizeGridCell 0 

mainLoop :: World -> S.Renderer -> IO ()
mainLoop w renderer = do
  events <- S.pollEvents
  t1 <- getCurrentTime
  let quit = elem S.QuitEvent $ map S.eventPayload events
  S.clear renderer
  S.rendererDrawColor renderer S.$= S.V4 19 19 19 255
  render w
  S.present renderer
  physicsStep w
  t2 <- getCurrentTime
  let diffTime = round $ (100000) * (diffUTCTime t2 t1)
  when (diffTime < 16667) $ do
    threadDelay (16667 - diffTime)
    unless (quit) (mainLoop w renderer)

parseopts :: Parser GameOpts
parseopts = GameOpts <$> switch
  ( long "debug"
    <> help "Turn on in-game debug hud" )