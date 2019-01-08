{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Ceph.Scene
import Ceph.Components
import Ceph.Physics
import Ceph.Physics.Box
import Ceph.Component.Enemy
import Ceph.Component.Weapon
import Ceph.Component.Player
import Ceph.Component.Projectile
import Ceph.Handler
import Ceph.Jams

import Apecs
import Data.List (zip4)
import Options.Applicative
import Graphics.Gloss
import Graphics.Gloss.Juicy
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
  set global ( Camera 0 2
             , Gravity $ V2 0 (-0.004)
             , mempty :: Beat)  
  -- make some euterpea sounds
  let e1 = c 4 wn 
      e2 = a 4 wn 
      e3 = g 4 wn
      m1 = transpose 3 $ e1
      m2 = transpose 3 $ e2
      m3 = transpose 3 $ e3
      n1 = instrument Flute $ m1 :=: m2 :=: m3
      n2 = transpose 2 n1
      n3 = transpose 5 n1
      l1 = tempo 1 $ Euterpea.line [n1,n2,n1,n2,n1,n2]
      l2 = transpose 8 l1
      l3 = transpose 3 l1 
      u1 = tempo 0.3 $ t1 2 Oboe
      u2 = tempo 0.3 $ t1 3 Bassoon
      am = [l1,u1,u2,n1,n2,n3]
      
      b1 = perc HiMidTom qn
      b2 = perc HiBongo qn
      b3 = perc HighFloorTom qn
      b4 = perc HiWoodBlock qn
      b5 = perc BassDrum1 qn
      b6 = perc LowMidTom qn
      b7 = perc LowBongo qn
      b8 = perc LowFloorTom qn
      b9 = perc LowWoodBlock qn
      b10 = perc LongWhistle qn
      bm = [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10]
     
  cm <- mapM ( M.decode . makeByteMusic ) am :: System World [M.Chunk]
  dm <- mapM ( M.decode . makeByteMusic ) bm :: System World [M.Chunk] 
  --read in sprites
  let handlePic = maybe
        (error "img not loading")
        return
  
  cig <- liftIO $ handlePic =<< loadJuicy "./resource/image/sword.png"
  hrpn <- liftIO $ handlePic =<< loadJuicy "./resource/image/harpoon.png"
  plante <- liftIO $ handlePic =<< loadJuicy "./resource/image/coral1.png" 
  grund <- liftIO $ handlePic =<< loadJuicy "./resource/image/ground.png"
  squid <- liftIO $ handlePic =<< loadJuicy "./resource/image/squid1.png"
  arw <- liftIO $ handlePic =<< loadJuicy "./resource/image/arrow.png"
  octo <- liftIO $ handlePic =<< loadJuicy "./resource/image/octo2.png"
  bults <- liftIO $ mapM (\b -> handlePic =<< loadJuicy b) ["./resource/image/bullet1.png","./resource/image/bullet2.png","./resource/image/bullet3.png"]
  
  
  blck3 <- liftIO  $ zip4 <$> randomDonutBox 700 600 900 <*> randomDonutBox 700 600 400 <*> replicateM 700 (randomRIO (20,30 :: Float)) <*> replicateM 700 (randomRIO (20,30 :: Float))
  let bl (r,s,g,a) =
        newEntity (Wall
                  , Position (V2 r s)
                  , Angle 0
                  , Velocity 0
                  , box ( V2 r s ) g a
                  , BodyPicture $ Scale (0.05 * g) (0.05 * a) plante
                  )
  mapM_ bl blck3
  sword cig
  harpoon hrpn
  player octo n3
  newEntity (Target 0)

  mapM_ (newArrow arw) $ concat . replicate 10 $ zip am cm
  mapM_ (newBullet bults) $ concat . replicate 10 $ zip am cm
            
  mapM_ (enemy squid) $ zip bm dm 
  newEntity ( Grid $ fromList [ (0, fromList [(0,())] ) ] )
  return ()

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

