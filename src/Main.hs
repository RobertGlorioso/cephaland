{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.List (isInfixOf,zip4)
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

--todo:
--refactor init script to delegate to various components

main :: IO ()
main = do
  S.initialize [S.InitVideo, S.InitAudio]
  M.initialize []
  M.openAudio M.defaultAudio 256
  window <- S.createWindow "CEPH" S.defaultWindow
  S.showWindow window
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  w <- initWorld
  runSystem (initGame renderer) w
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
  where 
    initGame r = do
      beatsFiles <- liftIO $ getDirectoryContents "./resource/sfx"
                    >>= return . filter ((\x y z -> x && y && z ) <$> isInfixOf "en" <*> isInfixOf ".wav"  <*> ( flip elem ['A'..'Z'] . head ))
      cols <- return $ makeColorsMidi (S.V4 0 50 0 0)
      cols2 <- return $ makeColorsMidi (S.V4 50 0 0 0) 
      cols3 <- return $ makeColorsMidi (S.V4 0 0 50 0)       
      msmpl <- return $ makeMidiSamples Tuba
      msmpl2 <- return $ makeMidiSamples Flute
      bsmpl <- return $ makeMidiBeats
      
      ifls <- return $ (++ ".wav") <$> makeMidiNames Accordion
      ifls2 <- return $ (++ ".wav") <$> makeMidiNames Flute 
      bcs <- liftIO $ mapM (\file -> M.load $ "./resource/sfx/" ++ file) beatsFiles :: System World [M.Chunk]
      pfs <- liftIO $ mapM (\file -> M.load $ "./resource/sfx/" ++ file) ifls :: System World [M.Chunk]
      pfs2 <- liftIO $ mapM (\file -> M.load $ "./resource/sfx/" ++ file) ifls2 :: System World [M.Chunk]
      sfxs <- return $ filter (\(SFXResources _ (Modify (_) (Prim (Note _ p))) _ ) -> p `elem` [(G,3),(A,4),(B,4),(C,4),(D,4),(E,4),(Fs,4)]) 
                $ getZipList $ SFXResources <$> ZipList pfs <*> ZipList msmpl <*> ZipList (fmap fst cols)
      sfxs2 <- return $ filter (\(SFXResources _ (Modify (_) (Prim (Note _ p))) _ ) -> p `elem` [(G,3),(A,4),(B,4),(C,4),(D,4),(E,4),(Fs,4)]) --[(E,3),(Fs,3),(G,4),(A,4),(B,4),(C,4),(D,4)]) 
                $ getZipList $ SFXResources <$> ZipList pfs2 <*> ZipList msmpl2 <*> ZipList (fmap fst cols2)
      sfxs3 <- return $ getZipList $ SFXResources <$> ZipList bcs <*> ZipList bsmpl <*> ZipList (fmap fst cols3)
      
      let enemyFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/squid1.bmp" 
      let chainFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/chain.bmp"
      let wallFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/wall.bmp"
      let wallFile2 = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/wall1.bmp"
      let wallFile3 = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/wall2.bmp"
      let floorFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/coral1.bmp"
      let octoFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/octo2.bmp"
      let bultFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/bullett.bmp"
      let netFile = "C:/Users/robertg/OneDrive/cephaland-master/resource/image/space.bmp"
      chnTexture <- liftIO $ loadTxtr r chainFile
      flrTexture <- liftIO $ loadTxtr r floorFile
      octoTexture <- liftIO $ loadTxtr r octoFile
      bultTexture <- liftIO $ loadTxtr r bultFile
      netTexture <- liftIO $ loadTxtr r netFile
      
      _ <- newSquall bultTexture (head sfxs)

      tg <- newEntity (Target 0,Box (0,0,0),Angle 0,Position 0)

      wallPos1 <- liftIO $
        zip4 <$>
          replicateM 50 (randomDonutBox 100 900)
          <*> replicateM 100 (randomDonutBox 600 400)
          <*> replicateM 100 (randomRIO (0,1 :: CDouble))
          <*> replicateM 100 (randomRIO (0,1 :: CDouble))
      wallEs1 <- mapM (\(w,bx) -> newWall r wallFile Wall1 w bx) $ zip wallPos1 (concat . repeat $ sfxs)
      wallPos2 <- liftIO $
        zip4 <$>
          replicateM 50 (randomDonutBox 600 900)
          <*> replicateM 100 (randomDonutBox 600 400)
          <*> replicateM 100 (randomRIO (0,1 :: CDouble))
          <*> replicateM 100 (randomRIO (0,1 :: CDouble))
      wallEs2 <- mapM (\(w,bx) -> newWall r wallFile2 Wall2 w bx) $ zip wallPos2 (concat . repeat $ sfxs2)
      wallPos3 <- liftIO $
        zip4 <$>
          replicateM 50 ( randomDonutBox 600 900 )
          <*> replicateM 100 ( randomDonutBox 600 400 )
          <*> replicateM 100 (randomRIO (0,1 :: CDouble))
          <*> replicateM 100 (randomRIO (0,1 :: CDouble))
      wallEs3 <- mapM (\(w,bx) -> newWall r wallFile3 Wall3 w bx) $ zip wallPos3 (concat . repeat $ sfxs3)
            
      mapM_ (newBullet bultTexture) sfxs
                  
      enms1 <- mapM (enemy r enemyFile Enemy1) sfxs
      mapM_ (enemy r enemyFile Enemy1) sfxs
      enms2 <- mapM (enemy r enemyFile Enemy1) sfxs2
      mapM_ (enemy r enemyFile Enemy1) sfxs2
      mapM_ (enemy r enemyFile Enemy1) sfxs3

      makeFloorWallBox flrTexture (pure 0) sfxs
      makeFloorWallBox flrTexture (V2 2600 0) sfxs2
      makeFloorWallBox flrTexture (V2 0 2600) sfxs3

      flip mapM_ (wallEs1 ++ enms1) $ \ent -> do
        SFXResources _ oldMp oldClr <- get ent
        u <- liftIO $ randomRIO (0,length msmpl - 1 )
        v <- liftIO $ randomRIO (0,length msmpl - 1 )
        newM <- liftIO $ midiLoad (msmpl !! u :+: msmpl !! v)
        ent `set` SFXResources newM oldMp oldClr

      -- _ <- oneWayWall (10,3) (head sfxs) flrTexture
      pl <- player1 octoTexture (head sfxs)
      let nr = MBoard (zip (repeat $ pure 0) $ sfxs ++ sfxs2 ++ sfxs3 ) True :: Netitor
      nt <- net netTexture nr

      wChains 0.6 (pl:nt:tg:[]) tg
      
      let makeChains i = replicateM i (chain' (head sfxs)) >>= (\chns -> chains (pl:chns) tg)
      
      mapM_ makeChains [10..20] 
      _ <- newEntity( listToBoard(wallEs1) )
      _ <- newEntity( listToBoard(wallEs2) )
      _ <- newEntity( listToBoard(wallEs3) )
      cmap (\bx -> bool Out In $ aabb bx (Box (0, 600, 600)))
      set global (Camera 0 1.0
                  , SDLRenderer r
                  , Beat 20 0
                  , mempty :: SCoord
                  , mempty :: SongList
                  , BoardControl Play Unlocked
                  )
      _ <- newEntity ( Grid mempty )
      playSong (Entity 3)
      randomizeGridCell 0 

mainLoop :: World -> S.Renderer -> IO ()
mainLoop w renderer = do
  events <- S.pollEvents
  t1 <- getCurrentTime
  let quit = elem S.QuitEvent $ map S.eventPayload events
  S.clear renderer
  S.rendererDrawColor renderer S.$= S.V4 190 190 19 255
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