{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Ceph.Scene
import Ceph.Components
import Ceph.Physics
import Ceph.Component.Enemy
import Ceph.Component.Sword
import Ceph.Handler
import Ceph.Jams


import Apecs
import Options.Applicative
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
--import Numeric.Hamilton hiding (System)

--import Numeric.LinearAlgebra.Static hiding (dim, (<>))
import Control.Monad
import System.Random
--import System.Directory
--import System.CPUTime
import Data.Monoid
import Data.List (isInfixOf)
import Data.IntMap (fromList)
--import GHC.TypeLits
import Linear (V2(..))
import qualified SDL.Mixer as M
import qualified SDL as S
import Euterpea
import Apecs.Util

--makeWorld "World2" [''Camera, ''BodyPicture, ''Player, ''Position, ''Velocity, ''Gravity, ''Angle, ''Target, ''Attacking, ''Charging, ''Charge, ''Dash, ''Projectile, ''ProjCount, ''Sword, ''Weapon, ''Enemy, ''Vitality, ''Health, ''Box, ''Resources, ''Wall, ''Debug, ''Behavior, ''Grid, ''ScreenBounds]

{--
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
  --}

main :: IO ()
main = do
  S.initialize [S.InitAudio]
  
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
  playIO (InWindow "CEPH" (640,480) (100,100)) (mixColors 0.1 0.9 blue black) 60 w (render o) (\e w -> runSystem (handle e) w >> return w) (stepper)
  M.closeAudio
  M.quit
  S.quit

parseopts = GameOpts <$> switch
  ( long "debug"
    <> help "Turn on in-game debug hud" )

initGame :: System World ()
initGame = do
  set global ( Camera 0 10
             , Gravity $ V2 0 (-0.001)
             , mempty :: Beat)  
  
 {--read in sfx
  --sound <- liftIO $ (M.load "test.wav") --"./resource/sfx/water.wav" :: IO M.Chunk)
  jams <- liftIO $  mapM M.load . filter ("Jam" `isInfixOf`) =<< getDirectoryContents "." :: System World [M.Chunk]
  ocarinas <- liftIO $  mapM M.load . filter ((&&) <$> ("Ocarina" `isInfixOf`) <*> ("C" `isInfixOf`)) =<< getDirectoryContents "." :: System World [M.Chunk]
  marimbas <- liftIO $  mapM M.load . filter ((&&) <$> ("Marimba" `isInfixOf`) <*> ("8" `isInfixOf`)) =<< getDirectoryContents "." :: System World [M.Chunk]

--}
  let m1 = c 3 wn -- :+: d 4 en :+: e 3 en
      m2 = d 3 wn
      --transpose 3 m1
      m3 = e 3 wn
      --invert m2 :+: retro m2

  --read in sprites
  let handlePic = maybe
        (error "img not loading")
        return
  
  cig <- liftIO $ handlePic =<< loadJuicy "./resource/image/sword.png"
  plante <- liftIO $ handlePic =<< loadJuicy "./resource/image/coral1.png" 
  grund <- liftIO $ handlePic =<< loadJuicy "./resource/image/ground.png"
  squid <- liftIO $ handlePic =<< loadJuicy "./resource/image/squid1.png"
  arw <- liftIO $ handlePic =<< loadJuicy "./resource/image/arrow.png"
  octo <- liftIO $ handlePic =<< loadJuicy "./resource/image/octo1.png"
  bults <- liftIO $ mapM (\b -> handlePic =<< loadJuicy b) ["./resource/image/bullet1.png","./resource/image/bullet2.png","./resource/image/bullet3.png"]
  sword cig
  
  let blck = do
        m <- randomRIO ((-70), 70 :: Int)
        y <- randomRIO ((-20), 20 :: Int)
        g <- randomRIO (30, 40 :: Int)
        a <- randomRIO (30, 40 :: Int)
        return $ [m,y,g,a]
  
  let blck2 = do
        m <- randomRIO ((-40), 40 :: Int)
        y <- randomRIO ((-70), 70 :: Int)
        g <- randomRIO (20, 30 :: Int)
        a <- randomRIO (20, 30 :: Int)
        return $ [m,y,g,a]

  gblx <- liftIO $ liftA2 (++) (replicateM 14 blck2) (replicateM 14 blck)
  let bl (fmap fromIntegral -> [r,s,g,a]) = wave (pure 0.05) (V2 r s) (0.1 * V2 g a)  plante
  mapM_ bl gblx
  
  mapM_ (enemy squid) $ concat $ replicate 4 [m1,m2,m3]
  {--replicateM 1 $ do
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
    --}

  cm <- M.decode $ makeByteMusic $ instrument Xylophone m3 :: System World M.Chunk
  newEntity (( Position (V2 0 50)
             , Velocity (V2 0 0)
             , BodyPicture $ scale 0.15 0.15 octo 
             , Box (0, 1, 1))
            , (ProjCount 30, Health 99)
            , Weapon Xylophone 
            , Resources [] [cm] 
            , Player
            , (NoBehavior, Charge 0.01 False))
  newEntity (Target 0)
  newEntity (Dash 0)
  newEntity ( Position 2000
            , Velocity 0 
            , Angle 0
            , NoBehavior
            , ( Arrow
              , Box (2000, 0.1, 0.07)
              , Resources [Scale 0.1 0.1 arw] [cm]
              )
            )
  newEntity ( Position 2000
            , Velocity 0 
            , Angle 0
            , Seek
            , ( Bullet
              , Box (2000, 0.01, 0.01)
              , Resources (fmap (Scale 0.02 0.02) bults) [cm]
              )
            )
  newEntity (Grid $ fromList [ (0, fromList [(0,())] ) ] )
  return ()

