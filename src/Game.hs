{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds                                #-}

import Ceph.Scene
import Ceph.Components
import Ceph.Physics
import Ceph.Physics.Box
import Ceph.Entity.Enemy
import Ceph.Entity.Sword
import Ceph.Handler
import Ceph.Jams

import Apecs
import Options.Applicative
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import Numeric.Hamilton hiding (System)

import Numeric.LinearAlgebra.Static hiding (dim, (<>))
import Control.Monad
import System.Random
import System.Directory
import System.CPUTime
import Data.Monoid
import Data.List (isInfixOf)
import GHC.TypeLits
import Linear (V2(..))
import qualified SDL.Mixer as M
import Euterpea
import Euterpea.IO.MIDI.ExportMidiFile
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B (toStrict)

makeInstrumentFiles inst = mapM_ (\ (i,j) -> writeMidi (show inst ++ show i ++ ".wav") $ j) $ zip [0..] $ instrument inst <$> [Prim $ Note t (n, o) | t <- [dwn] , n <- [minBound..maxBound :: PitchClass], o <- [3,4,5 :: Octave]]

makeJams = mapM_ (\ (i,j) -> writeMidi ("Jam" ++ show i ++ ".wav") $ j) $ zip [0..] $ t1 2 <$> [(Oboe)..(Ocarina)]


main' :: IO ()
main' = do
  M.openAudio M.defaultAudio 256
  --bm <- return . B.toStrict . toLazyByteString . buildMidi . toMidi . perform $ e 4 wn
  bm <- return . makeFile . toMidi . perform $ c 4 en :+: g 4 en :+: c 5 en :+: g 5 en
  (M.decode bm :: IO M.Chunk)  >>= M.playOn 0 M.Once >>  return () :: IO ()
  let while t f = do
        stop <- t
        if stop then f >> while t f else return ()
   
  while (M.playing 0) $ return ()
  print bm
  print =<< B.readFile "./test.wav"
  

main :: IO ()
main = do
  --SDL.initialize [SDL.InitAudio]
  
  M.openAudio M.defaultAudio 256
  w <- initWorld
  let opts = info (parseopts <**> helper)
        $ fullDesc
          <> progDesc "The Ceph Game"
          <> header "--debug turns on debugging" 
  o <- liftIO (execParser opts)
  runSystem (initGame) w
  render o w
  stepper (1/60) w
  playIO (InWindow "CEPH" (640,480) (100,100)) (mixColors 0.1 0.9 blue black) 60 w (render o) (\e w -> runSystem (handle e) w >> return w) stepper
  M.closeAudio
  M.quit
  --SDL.quit

parseopts = GameOpts <$> switch
  ( long "debug"
    <> help "Turn on in-game debug hud" )


initGame :: System World ()
initGame = do
  set global ( Camera 0 10
             , Gravity $ V2 0 (-0.001) )  
  
  --read in sfx
  --sound <- liftIO $ (M.load "test.wav") --"./resource/sfx/water.wav" :: IO M.Chunk)
  jams <- liftIO $  mapM M.load . filter ("Jam" `isInfixOf`) =<< getDirectoryContents "." :: System World [M.Chunk]
  ocarinas <- liftIO $  mapM M.load . filter ((&&) <$> ("Ocarina" `isInfixOf`) <*> ("C" `isInfixOf`)) =<< getDirectoryContents "." :: System World [M.Chunk]
  marimbas <- liftIO $  mapM M.load . filter ((&&) <$> ("Marimba" `isInfixOf`) <*> ("8" `isInfixOf`)) =<< getDirectoryContents "." :: System World [M.Chunk]
  let m1 = c 4 en :+: g 4 en :+: c 5 en :+: g 5 en
      m2 = m1 :+: transpose 3 m1
      m3 = m2 :+: m2 :+: invert m2 :+: retro m2
  [am,bm,cm] <- mapM (M.decode . makeFile . toMidi . perform) [m1,m2,m3] :: System World [M.Chunk]
  
  --read in sprites
  let handlePic = maybe
        (error "img not loading")
        return
  
  cig <- liftIO $ handlePic =<< loadJuicy "./resource/image/sword.png"
  plante <- liftIO $ handlePic =<< loadJuicy "../pixeditor/yourArt.png" 
  grund <- liftIO $ handlePic =<< loadJuicy "./resource/image/ground.png"
  arw <- liftIO $ handlePic =<< loadJuicy "./resource/image/arrow.png"
  octo <- liftIO $ handlePic =<< loadJuicy "./resource/image/octo1.png"
  sword cig

  let blck = do
        m <- randomRIO ((-500), 500 :: Int)
        y <- randomRIO ((-100), 100 :: Int)
        g <- randomRIO (1, 10 :: Int)
        a <- randomRIO (1, 10 :: Int)
        return $ [m,y,g,a]
  
  let blck2 = do
        m <- randomRIO ((-200), 200 :: Int)
        y <- randomRIO ((-200), 200 :: Int)
        g <- randomRIO (1, 10 :: Int)
        a <- randomRIO (1, 10 :: Int)
        return $ [m,y,g,a]

  let wall (V2 c1 c2) (V2 w h) s = newEntity (Wall, Angle 0, Position (V2 c1 c2), Box (V2 c1 c2, w, h), BodyPicture $ color red $ rectangleSolid (2*w) (2*h), Resources [] [s] )

  gblx <- liftIO $ liftA2 (++) (replicateM 150 blck2) (replicateM 100 blck)
  let bl [r,s,g,a] o = let r' = fromIntegral r; s' = fromIntegral s; g' = fromIntegral g; a' = fromIntegral a; in (wall (V2 r' s') (V2 g' a') o)
  mapM_ id $ zipWith bl gblx (concat $ repeat $ [am,bm,cm]) -- marimbas ++ ocarinas)
  replicateM 7 enemy
  replicateM 1 $ do
    g <- liftIO $ randomRIO (1, 10 :: Double)
    h <- liftIO $ randomRIO (1, 10 :: Double)

    let newBodyPhase = toPhase (twoBodySys 0.5 5) $ Cfg (vec2 2 0) (vec2 0 0.5)
    newEntity ( BodyPicture $ Scale 1 1 plante
              , (Position 0, box 0 0.1 0.1, Velocity 0, Angle 0)
              , OneBody 0 
              , PHS2 newBodyPhase
              , NoBehavior)
    
    newEntity ( BodyPicture $ Color green $ Circle 0.5
              , (Position 0, box 0 1 1, Velocity 0, Angle 0)
              , TwoBody 0
              , PHS2 newBodyPhase
              , NoBehavior)

    newEntity ( BodyPicture $ Color orange $ Circle 2
              , (Position 0, box 0 1 1, Velocity 0, Angle 0)
              , Pend (V2 (realToFrac g) (realToFrac h))
              , PHS $ toPhase pendSys $ Cfg (konst 40 :: R 1) (konst (h / 10) :: R 1)
              , NoBehavior)
                
  newEntity ( Position (V2 0 50)
            , Velocity (V2 0 0)
            , BodyPicture $ scale 0.15 0.15 octo --(color blue $ rectangleSolid 1 1)
            , (ProjCount 30, Health 99)
            , Box (0, 3, 3)
            , Resources [] [] --[sound]
            , Player
            , NoBehavior)
  newEntity (Target 0)
  newEntity (Dash 0)
  newEntity (Charge 0.01)
  return ()


