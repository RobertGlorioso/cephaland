{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Ceph.Scene where

import Ceph.Components
import Ceph.Scene.Camera
import Ceph.Scene.Board
import Ceph.Util

import Apecs
import Linear
import Control.Monad
import GHC.Word
import qualified SDL as S
import Foreign.C.Types
import System.Random
import Data.Vector.Storable (fromList)

{--addTrailPics :: Picture -> Velocity -> BodyPicture -> BodyPicture
addTrailPics p (Velocity (V2 x y)) (BodyPicture (Pictures ps)) =
  BodyPicture $ Pictures (p : (take 20 $ flip fmap ps $ \case
    (Translate x0 y0 p0) -> (Translate (x0-x) (y0-y) p0)
    --p0 -> p0
    ))

nextFrame :: (Animated,BodyPicture,Sprites) -> (Animated,BodyPicture,Sprites)
nextFrame (Loop,BodyPicture _,Sprites (p:ps)) = (Loop,BodyPicture p, Sprites (ps ++ [p]))
nextFrame (Animate 0,BodyPicture _,Sprites (p:ps)) = (Still, BodyPicture p, Sprites (ps ++ [p]))
nextFrame (Animate n,BodyPicture _,Sprites (p:ps)) = (Animate (n-1),BodyPicture p, Sprites (ps ++ [p]))
nextFrame (Still,b,s) = (Still,b,s)
nextFrame i@(_,_,Sprites []) = i
--}

renderTexture :: S.Renderer -> Txtr -> S.Rectangle CInt -> CDouble -> IO ()
renderTexture r (Txtr t size) clip n = S.copyEx r t (Just size) (Just clip) (180 * n / pi) Nothing (pure False)

checkerRect :: S.Renderer -> V2 CInt -> V2 CInt -> V4 Word8 -> Maybe  ( V4 Word8 ) -> System World ()
checkerRect r pos size color1 Nothing = do
  let (V2 sx sy) = size
  let (V2 ll lr) = pos 
  let (V2 ul ur) = pos + size
  S.rendererDrawColor r S.$= color1
  S.fillRects r $ fromList $ fmap fst $ filter snd $ zip [S.Rectangle (S.P $ V2 x y) (div <$> size <*> pure 4)| x <- [ll,ll+(sx `div` 4)..ul], y <- [lr,lr+(sy `div` 4)..ur]] (concat $ repeat [True, False])
checkerRect r pos size color1 (Just color2) = do
  let (V2 sx sy) = size
  let (V2 ll lr) = pos 
  let (V2 ul ur) = pos + size
  S.rendererDrawColor r S.$= color1
  mapM_ (\(p,t) -> when t $ S.fillRect r (Just $ S.Rectangle (S.P p) (div <$> size <*> pure 4)))
      $ zip [V2 x y | x <- [ll,ll+(sx `div` 4)..ul], y <- [lr,lr+(sy `div` 4)..ur]] (concat $ repeat [True, False])
  S.rendererDrawColor r S.$= color2
  mapM_ (\(p,t) -> when t $ S.fillRect r (Just $ S.Rectangle (S.P p) (div <$> size <*> pure 4)))
      $ zip [V2 x y | x <- [ll,ll+(sx `div` 4)..ul], y <- [lr,lr+(sy `div` 4)..ur]] (concat $ repeat [False,True])

randomChkrs :: S.Renderer -> Camera -> Position -> CInt -> V4 Word8 -> Maybe (V4 Word8) -> System World ()
randomChkrs r cam pos maxSize clr clr2 = do
  rpos <- liftIO $ (\o p -> Position $ V2 o p) <$> randomRIO (-10,10) <*> randomRIO (-10,10)
  bg <- liftIO $ randomRIO (6,maxSize)
  checkerRect r (round <$> applyView cam (rpos + pos) (pure bg)) (pure bg) clr clr2

renderEnt :: (Position, Angle, Txtr, Actor, Entity) -> S.Renderer -> System World ()
renderEnt (pos@(Position s@(V2 x2 y2)), Angle theta, pic, Weapon, e) r = do
  get e >>= \case
    Net -> do
      view <- get global :: System World Camera 
      cmapM_ $ \(Target x@(V2 x1 y1)) -> do
        if (abs (x2 - x1) < abs (y2 - y1)) then
          liftIO $ renderTexture r pic (S.Rectangle (S.P $ round <$> applyView view pos (round <$> s-x)) (round <$> pure 1.9 * (s-x)) ) (-pi/2 + v2ToRad (s-x))
          else
          liftIO $ renderTexture r pic (S.Rectangle (S.P $ round <$> applyView view pos (round <$> s-x)) (round <$> pure 1.9 * (s-x)) ) (v2ToRad (s-x))
    _ -> renderEnt (pos, Angle theta, pic, Wall, e) r 
renderEnt (pos, Angle theta, pic@(Txtr _ (S.Rectangle _ size)), _, _) r = do
  view <- get global :: System World Camera
  liftIO $ renderTexture r pic (S.Rectangle (S.P $ round <$> applyView view pos size) size) theta

applyView :: Camera -> Position -> V2 CInt -> V2 CDouble
applyView (Camera cam scale) (Position p) (S.V2 x y) = 
  (p - cam + (fromIntegral <$> ( div <$> S.V2 (negate x) (negate y) <*> pure 2 )) ) / pure scale  + S.V2 390 260

render :: World -> IO ()
render w = runWith  w $ do
  cmapM_ cameraFollowPlayer
  (SList slst :: SongList, SDLRenderer r, cam :: Camera, sc :: SCoord, Beat bm bt) <- get global
  --cmap nextFrame
  S.rendererDrawColor r S.$= S.V4 190 100 19 255
  cmapM_ $ \case
    (In,b) -> renderEnt b r
    _ -> return ()

  BoardControl _ boardlock <- get global
  S.rendererDrawColor r S.$= S.V4 150 150 150 255
  when (boardlock == Locked) $ do
    checkerRect r (pure 8) (V2 80 80) (V4 150 150 150 255) (Just $ V4 190 150 150 255) 
    
  chns <- flip cfoldM [] $ \b -> \case
    c@(Chain, Position _) -> return $ c:b
    _ -> return b
  
  flip mapM_ (zip slst chns)
    $ \(SFXResources _ _ (SpriteColor clr), (_,pos)) -> randomChkrs r cam pos 9 clr Nothing
  
  cmapM_ $ \case
    ((MBoard ns True) :: Netitor, Position p@(V2 px py)) -> do
      foldM_ (\(acc :: CDouble) (_,SFXResources _ _ (SpriteColor clr)) -> do
        S.rendererDrawColor r S.$= clr 
        let dsc = acc / 20
        cmapM_ $ \(Target t) -> do
          view <- get global
          S.fillRect r $ 
            Just (S.Rectangle 
              (S.P $ round <$> applyView view (Position $ S.V2 (px + (dsc * sin acc)) (py + ((dsc * cos acc)))) (round <$> t-p))
              (pure 3)
            )
        return $ acc + ((fromIntegral $ fromEnum sc) + (fromIntegral (bt + 10) / fromIntegral bm))
        ) (100) ns
    (MBoard _ False, Position p) -> return ()

  flip cfoldM_ 0 $ \acc (s :: Sequencer) -> do
    flip mapM_ s $ \(e :: Entity) -> do
      get e >>= 
        \case
          (pos,SFXResources _ _ (SpriteColor clr),In) -> replicateM_ 10 $ randomChkrs r cam pos 6 clr Nothing
          _ -> return ()
    soundBoard s sc >>=
      \m -> flip mapM_ m $ \case
        (False, Position (V2 x y), SpriteColor clr) 
          -> S.rendererDrawColor r S.$= clr 
            >> S.fillRect r (Just (S.Rectangle (S.P $ round <$> S.V2 (x+acc) y) (S.V2 15 15)))
        (True, Position (V2 x y), SpriteColor clr) 
          -> S.rendererDrawColor r S.$= clr
            >> S.fillRect r (Just (S.Rectangle (S.P $ round <$> S.V2 (x+acc) y) (S.V2 10 10)))
    return $ acc + 85
  
  S.rendererDrawColor r S.$= (S.V4 12 12 120 255 ) 
  

    

