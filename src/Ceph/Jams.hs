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
import Linear
import qualified Data.ByteString as Byte
import System.Process
import GHC.Word
import System.IO

midiLoad :: Music Pitch -> IO M.Chunk
midiLoad ms = do
  exportMidiFile "temp.mid" . toMidi . perform $ ms
  inF <- openFile "in.txt" ReadMode
  outF <- openFile "out.txt" WriteMode
  (_,_,_,process) <- createProcess 
    (shell "timidity -c \"./soundfonts/GeneralUser GS v1.471.cfg.txt\" -Ow temp.mid -o temp.wav" ) 
    {std_in = UseHandle inF, std_out = UseHandle outF, std_err = UseHandle outF}
  _ <- waitForProcess process
  sfx <- M.load "temp.wav"
  return sfx

makeColorsMidi :: V4 Word8 -> [(SpriteColor,Int)]
makeColorsMidi beg = 
  [((\(V3 x y z) -> SpriteColor $ beg + V4 x y z 255) $ g <$> h,i)
  | 
    h <- [V3 10 5 5 
          ,V3 5 5 10 
          ,V3 5 10 5 
          ,V3 15 15 10 
          ,V3 15 10 15 
          ,V3 10 15 15 
          ,V3 15 20 20
          ,V3 20 15 20 
          ,V3 20 20 15
          ,V3 25 20 25 
          ,V3 20 25 25 
          ,V3 25 25 20
          ,V3 40 35 35
          ,V3 35 40 35 
        ],
    --g <- [(+20),(+50),(+100),(+130)],
    g <- [(+50),(+100)],
    i <- [2,3]
  ]

makeMidiSamples :: InstrumentName -> [Music Pitch]
makeMidiSamples i = [instrument i $ n o p 
                    | n <- [a,as,b,bs,c,cs,d,ds,e,es,f,fs,g,gs]
                    , o <- [3,4]
                    , p <- [hn,wn] ]

makeMidiBeats :: [Music Pitch]
makeMidiBeats = [perc b p 
                | b <- [AcousticBassDrum .. OpenTriangle]
                , p <- [hn,wn] ]

makeMidiNames :: InstrumentName -> [String]
makeMidiNames i = [n ++ show o ++ p ++ show i
                  | n <- ["a","as","b","bs","c","cs","d","ds","e","es","f","fs","g","gs"]
                  , o <- [3,4]
                  , p <- ["hn","wn"] ]

makeMidiFiles :: InstrumentName -> IO ()
makeMidiFiles i = do
  mapM_ (\(file, mid) -> exportMidiFile file . toMidi . perform $ mid) 
      [(n ++ show o ++ p ++ show i ++ ".mid", instrument i $ m o q) 
      | (n,m) <- zip ["a","as","b","bs","c","cs","d","ds","e","es","f","fs","g","gs"] [a,as,b,bs,c,cs,d,ds,e,es,f,fs,g,gs], o <- [3..6], (p,q) <- zip ["sn", "en", "qn", "hn", "wn"] [sn,en,qn,hn,wn]]
      
  {--mapM_ (\(file, mid) -> Byte.writeFile file $ makeByteMusic mid) 
      [(show b ++ p ++ ".mid",perc b q) 
      | b <- [AcousticBassDrum .. OpenTriangle], (p,q) <- zip ["sn", "en", "qn", "hn", "wn"] [sn,en,qn,hn,wn]] --}

--reading in songs from a user specified file
--importSong :: String -> IO ( M.Chunk)
--importSong f = either (error) (M.decode . makeFile) =<< importFile f

succCycle :: SCoord -> SCoord
succCycle (SCoordF S4 SIV  _) = SCoordF S1 SI ()
succCycle (SCoordF i SIV  _) = SCoordF (succ i) SI ()
succCycle (SCoordF i j _) = SCoordF i (succ j) ()

incrementBeat :: System World ()
incrementBeat = do
  (Beat m i, BoardControl boardstat _) <- get global
  -- plays sound effects on beat
  when (boardstat == Play) $
    if (m <= i) then (global `set` Beat m 0) >> cmapM_ ( \case
        (Sing, actr :: Actor, ent) -> playSong ent >> if ( actr == Weapon ) then ( ent `set` Seek ) else ent `set` NoBehavior
        _ -> return ()
        ) >> (global `modify` (\s  -> succCycle s :: SCoord)) 
    else global `set` Beat m (i+1) 
{--liftIO . print =<< flip cfoldM (Song (rest 0)) (\s@(Song i) ->
            (\case
                (Sing, Song j) -> return (Song $ i :=: j)
                _ -> return (Song i)
            )
          )
  -- e `set` (Debug . show $ (a, Song i)) >>
--}

--this function assigns a sound to the first open channel it finds
playSong :: Entity -> System World ()
playSong ent = do
  (SFXResources p _ _) <- get ent
  soundPlay [p] 0
  where
    soundPlay [] _ = return ()
    soundPlay ms@(m:mz) i 
      | i > 7 = M.halt (7) >> M.playOn 0 M.Once m >> soundPlay mz 1
      | otherwise = do
        chanAvailable <- not <$> M.playing i
        if chanAvailable then M.playOn i M.Once m >> soundPlay mz (i+1) else soundPlay ms (i+1)
        
{--

--this was all stolen from HSoM
data DetGrammar a = DetGrammar  a           --  start symbol
                                [(a,[a])]   --  productions
  deriving Show

detGenerate :: Eq a => DetGrammar a -> [[a]]
detGenerate (DetGrammar st ps) = iterate (concatMap f) [st]
            where f a = maybe [a] id (lookup a ps)

redAlgae = DetGrammar 'a'
               [  ('a',"b|c"),   ('b',"b"),  ('c',"b|d"),
                  ('d',"e\\d"),  ('e',"f"),  ('f',"g"),
                  ('g',"h(a)"),  ('h',"h"),  ('|',"|"),
                  ('(',"("),     (')',")"),  ('/',"\\"),
                  ('\\',"/")
               ]
               

t n g = sequence_ (map putStrLn (take n (detGenerate g)))

data Grammar a = Grammar  a          --  start sentence
                          (Rules a)  --  production rules
     deriving Show

data Rules a  =  Uni  [Rule a] 
              |  Sto  [(Rule a, Prob)]
     deriving (Eq, Ord, Show)

data Rule a = Rule { lhs :: a, rhs :: a }
     deriving (Eq, Ord, Show)

type Prob = Double
type ReplFun a  = [[(Rule a, Prob)]] -> (a, [Rand]) -> (a, [Rand])
type Rand       = Double

gen :: Ord a => ReplFun a -> Grammar a -> Int -> [a]
gen f (Grammar s rules) seed = 
    let  Sto newRules  = toStoRules rules
         rands         = randomRs (0.0,1.0) (mkStdGen seed)
    in  if checkProbs newRules
        then generate f newRules (s,rands)
        else (error "Stochastic rule-set is malformed.")

toStoRules :: (Ord a, Eq a) => Rules a -> Rules a  
toStoRules (Sto rs)  = Sto rs
toStoRules (Uni rs)  = 
  let rs' = groupBy (\r1 r2 -> lhs r1 == lhs r2) (sort rs)
  in Sto (concatMap insertProb rs')

insertProb :: [a] -> [(a, Prob)] 
insertProb rules =  let prb = 1.0 / fromIntegral (length rules)
                    in zip rules (repeat prb)

checkProbs :: (Ord a, Eq a) => [(Rule a, Prob)] -> Bool
checkProbs rs = and (map checkSum (groupBy sameLHS (sort rs)))

eps = 0.001 

checkSum :: [(Rule a, Prob)] -> Bool 
checkSum rules =  let mySum = sum (map snd rules)
                  in abs (1.0 - mySum) <= eps 

sameLHS :: Eq a => (Rule a, Prob) -> (Rule a, Prob) -> Bool 
sameLHS (r1,f1) (r2,f2) = lhs r1 == lhs r2

generate ::  Eq a =>  
             ReplFun a -> [(Rule a, Prob)] -> (a,[Rand]) -> [a] 
generate f rules xs = 
  let  newRules      =  map probDist (groupBy sameLHS rules)
       probDist rrs  =  let (rs,ps) = unzip rrs
                        in zip rs (tail (scanl (+) 0 ps))
  in map fst (iterate (f newRules) xs)

data LSys a  =  N a 
             |  LSys a   :+   LSys a 
             |  LSys a   :.   LSys a 
             |  Id 
     deriving (Eq, Ord, Show) 

replFun :: Eq a => ReplFun (LSys a)
replFun rules (s, rands) =
  case s of
    a :+ b  ->  let  (a',rands')   = replFun rules (a, rands )
                     (b',rands'')  = replFun rules (b, rands')
                in (a' :+ b', rands'')
    a :. b  ->  let  (a',rands')   = replFun rules (a, rands )
                     (b',rands'')  = replFun rules (b, rands')
                in (a' :. b', rands'')
    Id      ->  (Id, rands)
    N x     ->  (getNewRHS rules (N x) (head rands), tail rands)

getNewRHS :: Eq a => [[(Rule a, Prob)]] -> a -> Rand -> a
getNewRHS rrs ls rand = 
  let  loop ((r,p):rs)  = if rand <= p then rhs r else loop rs
       loop []          = error "getNewRHS anomaly"
  in case (find (\ ((r,p):_) -> lhs r == ls) rrs) of
        Just rs  -> loop rs
        Nothing  -> error "No rule match"

type IR a b = [(a, Music b -> Music b)]  --  ``interpetation rules'' 

interpret :: (Eq a) => LSys a -> IR a b -> Music b -> Music b
interpret (a :. b)  r m = interpret a r (interpret b r m)  
interpret (a :+ b)  r m = interpret a r m :+: interpret b r m
interpret Id        r m = m 
interpret (N x)     r m = case (lookup x r) of
                            Just f   -> f m
                            Nothing  -> error "No interpetation rule"

data LFun = Inc | Dec | Same
     deriving (Eq, Ord, Show)

ir :: IR LFun Pitch
ir = [ (Inc, transpose 3),
       (Dec, transpose (-3)),
       (Same, id)]

inc, dec, same :: LSys LFun
inc   = N Inc
dec   = N Dec
same  = N Same

sc = inc :+ dec

r1a  = Rule inc (sc :. sc)
r1b  = Rule inc sc
r2a  = Rule dec (sc :. sc)
r2b  = Rule dec sc
r3a  = Rule same inc
r3b  = Rule same dec
r3c  = Rule same same

g1 = Grammar same (Uni [r1b, r1a, r2b, r2a, r3a, r3b])

t1 n i =  instrument i $ interpret (gen replFun g1 n !! 3) ir (c 5 tn)


-------------midi encode/decode------------

makeByteMusic = ( makeFile . toMidi . perform) :: Music Pitch -> Byte.ByteString

makeFile :: Midi -> Byte.ByteString
makeFile (Midi ft td trs) = 
    let ticksPerQn = 
            case td of TicksPerBeat x -> x
                       TicksPerSecond x y -> 
                           error ("(makeFile) Don't know how "++
                           "to handle TicksPerSecond yet.")
        header = makeHeader ft (length trs) ticksPerQn
        body = map makeTrack trs
    in  Byte.concat (header:body)


midiHeaderConst :: Byte.ByteString
midiHeaderConst = 
    Byte.pack [0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06] 

type TrackCount = Int
type TicksPerQN = Int


makeHeader :: FileType -> TrackCount -> TicksPerQN -> Byte.ByteString
makeHeader ft numTracks ticksPerQn = 
    let 
        ft' = case ft of SingleTrack -> [0x00, 0x00]
                         MultiTrack -> [0x00, 0x01]
                         MultiPattern -> error ("(makeHeader) Don't know "++
                                         "how to handle multi-pattern yet.")
        numTracks' = padByte 2 numTracks
        ticksPerQn' = padByte 2 ticksPerQn
    in  if numTracks > 16 then error ("(makeHeader) Don't know how to "++
                               "handle >16 tracks!")
        else Byte.concat [midiHeaderConst, Byte.pack ft', numTracks', ticksPerQn']

padByte :: Integral a => Int -> a -> Byte.ByteString
padByte byteCount i = 
  let b = Byte.pack [fromIntegral i] 
      n = Byte.length b
      padding = Byte.pack $ take (fromIntegral $ byteCount - n) $ repeat 0x00
  in  if n < byteCount then Byte.concat [padding, b] else b


makeTrack :: Track Ticks -> Byte.ByteString
makeTrack t = 
    let body = makeTrackBody t
        header = makeTrackHeader body
    in  Byte.concat [header, body]
trackHeaderConst :: Byte.ByteString
trackHeaderConst = Byte.pack [0x4D, 0x54, 0x72, 0x6B] 
makeTrackHeader :: Byte.ByteString -> Byte.ByteString
makeTrackHeader tbody = 
    let len = Byte.length tbody
        f = Byte.pack . map (fromIntegral . binStrToNum . reverse) . 
            breakBinStrs 8 . pad (8*4) '0' . numToBinStr
    in  Byte.concat [trackHeaderConst, f len]
----rack events have two components: a variable-length delta-time and
-- message. The delta-time is the number of ticks between the last 
--essage and the next one. The format will be: time message time message ...
----owever, delta-times are tricky things. The fact that they can be 
--ny length requires that they be encoded in a special way. The binary
--alue of the number is split into 7-bit sections. This splitting 
--oes from RIGHT TO LEFT (this is not in any documentation I have read,
--ut was the only way that worked). For n sections, the first start 
--ith a 1 and the last starts with a 0 - thereby indicating the last 
--yte of the number. The following is an example of the conversion:
----92 track ticks = C0 (hex) = 1100 0000 (bin) 
----plit into 7-bit groups:        [1]  [100 0000]
--pply padding:           [000 0001]  [100 0000]
--dd flags:              [1000 0001] [0100 0000]
--esult as hex               8    1      4    0
makeTrackBody :: Track Ticks -> Byte.ByteString 
makeTrackBody [] = endOfTrack -- end marker, very important!
makeTrackBody ((ticks, msg):rest) = 
    let b = msgToBytes msg
        b' = [to7Bits ticks, msgToBytes msg, makeTrackBody rest]
    in  if Byte.length b > 0 then Byte.concat b'             
        else makeTrackBody rest
----he end of track marker is set 96 ticks after the last event in the 
--rack. This offset is arbitrary, but it helps avoid clipping the notes
--t the end of a file during playback in a program like Winamp or
--uicktime.
endOfTrack = Byte.concat [to7Bits 96, Byte.pack [0xFF, 0x2F, 0x00]]
----plitting numbers into 7-bit sections and applying flags is done
--y the following process:
-- convert to a binary string representation
-- pad the number to be full bytes
-- split from right to left into groups of 7 and apply flags
-- convert each 8-bit chunk back to a byte representation
to7Bits :: (Integral a, Show a) => a -> Byte.ByteString
to7Bits =  Byte.pack . map (fromIntegral . binStrToNum . reverse) .
           fixBinStrs . map (padTo 7 . reverse). reverse . 
           breakBinStrs 7 . reverse . padTo 7 . numToBinStr
----ad a binary string to be a multiple of i bits:
padTo :: Int -> String -> String
padTo i xs = if length xs `mod` i == 0 then xs else padTo i ('0':xs)
----reak a string into chunks of length i:
breakBinStrs :: Int -> String -> [String]
breakBinStrs i s = if length s <= i then [s] else take i s : breakBinStrs i (drop i s)
----onvert a number to a binary string:
numToBinStr :: (Integral a, Show a) => a -> String
numToBinStr i = showIntAtBase 2 intToDigit i ""
----onvert a binary string to an integer:
binStrToNum :: String -> Int
binStrToNum [] = 0
binStrToNum ('0':xs) = 2* binStrToNum xs
binStrToNum ('1':xs) = 1 + 2*binStrToNum xs
binStrToNum _ = error "bad data."
----ppend flags to a string (note, the string must be BACKWARDS):
fixBinStrs :: [String] -> [String]
fixBinStrs xs = 
    let n = length xs
        bits = take (n-1) (repeat '1') ++ "0"
    in  Prelude.zipWith (:) bits xs
----ad a list from the left until it is a fixed length:
pad :: Int -> a -> [a] -> [a]
pad b x xs = if length xs >= b then xs else pad b x (x:xs)
msgToBytes :: Message -> Byte.ByteString
msgToBytes (NoteOn c k v) = 
    Byte.concat [Byte.pack [0x90 + fromIntegral c], padByte 1 k, padByte 1 v]
msgToBytes (NoteOff c k v) = 
    Byte.concat [Byte.pack [0x80 + fromIntegral c], padByte 1 k, padByte 1 v]
msgToBytes (ProgramChange c p) =  
    Byte.concat [Byte.pack [0xC0 + fromIntegral c], padByte 1 p]
msgToBytes (ControlChange c n v) =  
    Byte.concat [Byte.pack [0xB0 + fromIntegral c], padByte 1 n, padByte 1 v]
msgToBytes (TempoChange t) = -- META EVENT, HAS NO CHANNEL NUMBER
    Byte.concat [Byte.pack [0xFF, 0x51, 0x03], fixTempo t]
msgToBytes x = Byte.empty -- error ("(msgToBytes) Message type not currently "++ 
--               "supported: "++show x)
----ix a tempo value to be exactly 3 bytes:
fixTempo = Byte.pack . map (fromIntegral . binStrToNum . reverse) . 
           breakBinStrs 8 . pad (4*6) '0' . numToBinStr
exportMidiFile :: FilePath -> Midi -> IO ()
exportMidiFile fn = Byte.writeFile fn . makeFile
--}