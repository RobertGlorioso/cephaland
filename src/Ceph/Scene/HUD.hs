{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Ceph.Scene.HUD where

import Ceph.Scene.Board
import Ceph.Scene.Camera
import Ceph.Physics.Box
import Ceph.Components
import Ceph.Util
import Ceph.Jams
import Data.List
import GHC.Word
import Control.Monad
import Linear (V2(..))
import Foreign.C.Types
import System.Random
import Data.Vector.Storable (fromList)
import Data.Functor.Rep
import Euterpea (InstrumentName(..),PitchClass(..))
import SDL hiding (Locked, get, Play)
import qualified SDL.Mixer as M
import Apecs hiding (($=))

drawRect :: Renderer -> V2 CInt -> V2 CInt -> SpriteColor -> System World ()
drawRect r pos (V2 sx sy) (SpriteColor c) = do 
  rendererDrawColor r $= c
  fillRect r (Just $ Rectangle (P pos) (V2 sx sy))

stripeRect :: Renderer -> V2 CInt -> V2 CInt -> [SpriteColor] ->  System World ()
stripeRect r pos size scs = do
  let (V2 sx sy) = size
  let (V2 ll lr) = pos 
  let (V2 _ ur) = pos + size
  let l = fromIntegral $ length scs
  flip mapM_ (zip [V2 ll y | y <- [lr,lr+(sy `div` l)..ur]] scs)
    $ \(p,SpriteColor c) -> do
      rendererDrawColor r $= c
      fillRect r (Just $ Rectangle (P p) (V2 sx (sy `div` l)))
    
checkerRect :: Renderer -> V2 CInt -> V2 CInt -> SpriteColor -> (Maybe SpriteColor) -> System World ()
checkerRect r pos size (SpriteColor color1) Nothing = do
  let (V2 sx sy) = size
  let (V2 ll lr) = pos 
  let (V2 ul ur) = pos + size
  when (sx < 4 || sy < 4) $ error "checker size too small"
  rendererDrawColor r $= color1
  fillRects r $ fromList $ fmap fst $ filter snd $ zip [Rectangle (P $ V2 x y) (div <$> size <*> pure 4)| x <- [ll,ll+(sx `div` 4)..ul], y <- [lr,lr+(sy `div` 4)..ur]] (concat $ repeat [True, False])
checkerRect r pos size (SpriteColor color1) (Just (SpriteColor color2)) = do
  let (V2 sx sy) = size
  let (V2 ll lr) = pos 
  let (V2 ul ur) = pos + size
  when (sx < 4 || sy < 4) $ error "checker size too small"
  rendererDrawColor r $= color1
  mapM_ (\(p,t) -> when t $ fillRect r (Just $ Rectangle (P p) (div <$> size <*> pure 4)))
      $ zip [V2 x y | x <- [ll,ll+(sx `div` 4)..ul], y <- [lr,lr+(sy `div` 4)..ur]] (concat $ repeat [True, False])
  rendererDrawColor r $= color2
  mapM_ (\(p,t) -> when t $ fillRect r (Just $ Rectangle (P p) (div <$> size <*> pure 4)))
      $ zip [V2 x y | x <- [ll,ll+(sx `div` 4)..ul], y <- [lr,lr+(sy `div` 4)..ur]] (concat $ repeat [False,True])

randomChkrs :: Renderer -> Camera -> Position -> CInt -> SpriteColor -> (Maybe SpriteColor) -> System World ()
randomChkrs r cam pos maxSize col1 col2 = do
  rpos <- liftIO $ (\o p -> Position $ V2 o p) <$> randomRIO (-10,10) <*> randomRIO (-10,10)
  bg <- liftIO $ randomRIO (6,maxSize)
  checkerRect r (round <$> applyView cam (rpos + pos) (pure bg)) (pure bg) col1 col2

makeSequencer :: Renderer -> SCoord -> V2 CDouble -> System World ()
makeSequencer r sc mc = flip cfoldM_ 0 $ \acc (s :: Sequencer) -> do
  {--flip mapM_ s $ \(e :: Entity) -> do
    get e >>= 
      \case
        (pos,SFXResources _ _ c,In) -> replicateM_ 4 $ randomChkrs r cam pos 6 c Nothing
        _ -> return ()--}
  
  soundBoard s sc >>=
    \m -> flip mapM_ m $ \case
      (False, Position ps@(V2 x y), scs) 
        -> do
          when (aabb (Box (ps + V2 (acc + 6) (-3),12,12)) (Box (mc,1,1))) $ do
            rendererDrawColor r $= (V4 240 240 0 255)
            fillRect r (Just (Rectangle (P $ round <$> pure (-2) + V2 (x+acc) y) (pure 19)))
          stripeRect r (round <$> V2 (x+acc) y) (V2 15 15) scs
      (True, Position ps@(V2 x y), scs) 
        -> do
          when (aabb (Box (ps + V2 (acc + 6) (-3),9,9)) (Box (mc,1,1))) $ do
            rendererDrawColor r $= (V4 240 240 0 255)
            fillRect r (Just (Rectangle (P $ round <$> V2 (x+acc) y) (pure 14)))
          stripeRect r (round <$> pure 2 + V2 (x+acc) y) (V2 10 10) scs
  return $ acc + 85

boardLight :: Renderer -> BoardControl -> System World ()
boardLight r bc = do
  if (lock bc /= Locked) 
    then rendererDrawColor r $= (pure 255) >> fillRect r (Just $ Rectangle (P $ pure 9) (pure 7))
    else rendererDrawColor r $= (pure 255) >> do
      fillRect r (Just $ Rectangle (P (V2 9 9)) (V2 7 3))
      fillRect r (Just $ Rectangle (P (V2 9 13)) (V2 7 3))
  let col = if status bc == Play then V4 50 250 50 255 else V4 250 50 50 255 
  flip mapM_ (zip [S1,S2,S3,S4] [25,45,65,85]) $ \(s,y) -> do
    when (s `elem` playback bc) $ rendererDrawColor r $= col >> fillRect r (Just $ Rectangle (P (V2 10 y)) (pure 3))

playingLight :: Renderer -> System World ()
playingLight r = 
  flip mapM_ [0..127] $ \i -> do
    isPlaying <- M.playing $ fromIntegral i
    if isPlaying then rendererDrawColor r $= V4 50 250 50 255 >> fillRect r (Just $ Rectangle (P (V2 (360 + 3*i) 10)) (pure 2))
      else  rendererDrawColor r $= V4 150 150 150 255 >> fillRect r (Just $ Rectangle (P (V2 (360 + 3*i) 10)) (pure 2))

adjustSequencer :: Renderer -> BoardControl -> SCoord -> System World ()
adjustSequencer r bdctrl sc = 
  cmapM_ $ \case
    (Sing, ins1 :: InstrumentName, ent) -> cmap $ \ (ins2 :: InstrumentName, s :: Sequencer) ->
      if lock bdctrl == Locked && ins1 == ins2 then fmap nub $ modifyBoard s sc (const (ent:)) else
        if ins1 == ins2 then fmap nub $ updateBoard s sc [ent] else s
    _ -> return ()

fillMBoardBoxes :: Renderer -> Camera -> System World ()
fillMBoardBoxes r cam = 
  cmapM_ $ \case
    (MBoard ns True :: Netitor) -> do
      flip mapM_ ns $ \(Box (p,w,h),e) -> do
        (sfx,Position pos,scope) <- get e
        let SFXResources _ sng (SpriteColor c) = sfx
        rendererDrawColor r $= c
        let mBoardRect = fillRect r $ 
                                Just $ Rectangle 
                                  (P $ round <$> applyView cam (Position pos) (round <$> 2 * V2 w h))
                                  (round <$> 2 * V2 w h)
        when (scope == In) $ mBoardRect
    (MBoard _ False) -> return ()

makeNet :: Renderer -> Camera -> Beat -> SCoord -> System World ()
makeNet r cam (Beat bm bt) sc = 
  cmapM_ $ \case
    ((MBoard ns True) :: Netitor, (Position p@(V2 px py))) -> do
      foldM_ (\(acc :: CDouble) (_,e) -> do
        (beh :: Behavior, sfx, pos :: Position) <- get e
        let SFXResources _ sng (SpriteColor c) = sfx
        rendererDrawColor r $= ((\(V4 a b c _) -> V4 a b c 0) c)
        cmapM_ $ \(Target _, Position t) -> do
          let V2 dsx dsy = (acc / 7500 *) <$> (t-p)
          let newP = 
                case getInst sng of
                  Percussion -> rotate_pos_cw (Position $ V2 (px + (dsx * sin acc)) (py + (dsy * cos acc))) (Box (p,0,0), Angle $ pi / 2 - v2ToRad (t-p))
                  Oboe -> rotate_pos_cw (Position $ V2 (px + (dsx * sin acc)) (py + (dsy * cos acc))) (Box (p,0,0), Angle $ v2ToRad (t-p))
                  _ -> rotate_pos_cw (Position $ V2 (px + (dsx * sin acc)) (py + (dsy * cos acc))) (Box (p,0,0), Angle $ pi / 2 + v2ToRad (t-p))
          let netRect s = fillRect r $ 
                            Just $ Rectangle 
                              (P $ round <$> applyView cam newP (round <$> t-p))
                              s
          when (beh == Sing) $ do
            rendererDrawColor r $= c
            netRect $ pure 4
            rendererDrawColor r $= ((\(V4 a b c _) -> V4 a b c 1) c)         
          netRect $ pure 2
        return $ acc + ((fromIntegral $ fromEnum sc) + (fromIntegral (bt) / fromIntegral (bm)))
        ) (400) ns
    (MBoard _ False, _) -> return ()

makeClock :: Renderer -> SCoord -> System World ()
makeClock r sc = cmapM_ 
  $ \(mclock :: MusicClock, Position pos) -> do
    (currentSfx :: [[SFXResources]]) <- flip cfoldM [] $ \a (sequencer :: Sequencer) -> do
                      (sfx :: [SFXResources]) <- mapM get $ index sequencer (sc)
                      return $ sfx:a
    currentSing <- flip cfoldM [] $ \a (s, inst, sfx) -> if s == Sing && inst /= Percussion then return $ sfx:a else return a
    flip mapM_ mclock $ \(s,Position p) -> do
      let secColor cs = case getInst (song cs) /= Percussion && getNote (song cs) == getNote s of
                      True -> Just $ clr cs
                      False -> Nothing
      
      uncurry (checkerRect r (round <$> 40*p + pos) (pure 8)) 
        $ foldl (\j cs -> case j of 
                            (blk@(SpriteColor (V4 0 0 0 0)), Nothing) -> (maybe blk id $ secColor cs, Nothing)
                            (fc, Nothing) -> (fc, secColor cs)
                            jj -> jj
          ) (SpriteColor $ pure 0,Nothing) (nub $ (concat currentSfx) ++ currentSing)
  
renderHUD :: Renderer -> System World ()
renderHUD r = do 
  (sc, MCoordF (Box (mc,_,_)) selectedEnt :: MCoord, bdctrl, beat :: Beat) <- get global
  global `modify` \bc@(BoardControl _ _ i _) -> if i <= 150 then bc { charge = i + 15 } else bc 
  boardLight r bdctrl
  playingLight r
  makeSequencer r sc mc
  --makeNet r cam beat sc
  adjustSequencer r bdctrl sc 
  makeClock r sc 
  