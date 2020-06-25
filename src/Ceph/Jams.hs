module Ceph.Jams where

import Ceph.Components

import Apecs
import qualified SDL.Mixer as M
import Euterpea
import Data.List hiding (transpose)
import Codec.Midi
import System.Random
import Control.Monad
import Data.Char
import Data.List
import Linear
import qualified Data.ByteString as Byte
import System.Process
import GHC.Word
import Options.Applicative
import System.IO

makeSamples :: InstrumentName -> System World ()
makeSamples instr = do
  msmpl <- return $ makeMidiSamples instr
  cols <- return $ makeColorsMidi (V4 0 50 0 0)
  pfs <- liftIO $ mapM midiLoad msmpl
  instrumentSfxs1 <- return $ getZipList $ SFXResources <$> ZipList pfs <*> ZipList msmpl <*> ZipList (cols)
  return ()

midiWrite :: FilePath -> Music Pitch -> IO (M.Chunk)
midiWrite fp ms = do
  mp <- return . toMidi . perform $ ms
  exportMidiFile fp mp
  midiLoad ms
  

midiLoad :: Music Pitch -> IO M.Chunk
midiLoad ms = do
  exportMidiFile "temp.mid" . toMidi . perform $ ms
  inF <- openFile "in.txt" ReadMode
  outF <- openFile "out.txt" WriteMode
  (_,_,_,process) <- createProcess 
    (shell "timidity -c \"./soundfonts/GeneralUser GS v1.471.cfg.txt\" -Ow temp.mid -o temp.wav") 
    {std_in = UseHandle inF, std_out = UseHandle outF, std_err = UseHandle outF}
  _ <- waitForProcess process
  sfx <- M.load "temp.wav"
  return sfx

makeColorsMidi :: V4 Word8 -> [SpriteColor]
makeColorsMidi beg = 
  [(\(V3 x y z) -> SpriteColor $ beg + V4 x y z 255) $ f <$> g <$> h
  | g <- [(+25),(+60),(+100),(+140)],
    f <- [(+1)],
    h <- [V3 10 5 10 
          ,V3 15 10 15 
          ,V3 20 15 15 
          ,V3 15 20 20 
          ,V3 20 15 20 
          ,V3 25 20 20 
          ,V3 25 25 20
          ,V3 30 25 30
          ,V3 30 30 35 
          ,V3 35 35 40
          ,V3 40 45 40
          ,V3 45 40 45 
        ]
  ] ++ [SpriteColor $ V4 255 255 255 255]

makeMidiSamples :: InstrumentName -> [Music Pitch]
makeMidiSamples i = [instrument i $ n o p 
                    | o <- [2,3,4,5]
                    , p <- [qn]
                    , n <- [af,a,bf,b,c,cs,d,ef,e,f,fs,g] ] ++  [instrument i $ rest qn]

makeCoFSamples :: InstrumentName -> [Music Pitch]
makeCoFSamples i = [instrument i $ n o p 
                    | o <- [2,3,4,5]
                    , p <- [qn]
                    , n <- [f,c,g,d,a,e,b,fs,cs,af,ef,bf] ] ++  [instrument i $ rest qn]

makeMidiBeats :: [Music Pitch]
makeMidiBeats = [perc b p 
                | b <- [AcousticBassDrum .. OpenTriangle]
                , p <- [qn] ] ++ [instrument Percussion $ rest qn]

makeMidiNames :: InstrumentName -> [String]
makeMidiNames i = [n ++ show o ++ p ++ show i
                  | o <- [3,4]
                  , p <- ["qn"]
                  , n <- ["af","a","bf","b","c","cs","d","ef","e","f","fs","g"]
                  ]

--reading in songs from a user specified file
--importSong :: String -> IO ( M.Chunk)
--importSong f = either (error) (M.decode . makeFile) =<< importFile f

succCycle :: SCoord -> SCoord
succCycle (SCoordF S4 SIV  _) = SCoordF S1 SI ()
succCycle (SCoordF i SIV  _) = SCoordF (succ i) SI ()
succCycle (SCoordF i j _) = SCoordF i (succ j) ()

srowCycle :: SRow -> SRow
srowCycle S4 = S1
srowCycle s = succ s

incrementBeat :: System World ()
incrementBeat = do
  (Beat m i, BoardControl _ _ _ bp, s@(SCoordF sr sc ())) <- get global
  -- plays sound effects on beat
  when (bp /= []) $
    if (m <= i) then
      if (sr `elem` bp) then do
        let newS@(SCoordF newSr newSc _) = succCycle s
        global `set` if (newSr `elem` bp) then newS else SCoordF (head $ dropWhile (`notElem` bp) $ iterate srowCycle newSr) newSc ()
        global `set` Beat m 0 
        cmapM_ $ \case
          (Sing, actr :: Actor, SFXResources p _ _, e) -> e `set` NoBehavior >> soundPlay [p] 0
          _ -> return ()
        else global `set` SCoordF (head $ dropWhile (`notElem` bp) $ iterate srowCycle sr) sc () 
      else global `set` Beat m (i+1) 
soundPlay [] _ = return ()
soundPlay ms@(m:mz) i 
      | i > 127 = M.halt (59) >> M.playOn 127 M.Once m >> soundPlay mz 1
      | otherwise = do
        chanAvailable <- not <$> M.playing i
        if chanAvailable then M.playOn i M.Once m >> soundPlay mz (i+1) else soundPlay ms (i+1)
        