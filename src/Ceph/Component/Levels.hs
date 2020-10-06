{-# LANGUAGE ViewPatterns #-}
module Ceph.Component.Levels where

import qualified Data.IntMap as M
import Data.List (head)
import Ceph.Components
import Ceph.Jams
import Ceph.Util
import Ceph.Physics.Box
import Ceph.Scene.Board
import Options.Applicative
import Control.Monad
import Apecs
import Linear
import Foreign.C.Types
import Euterpea 

seqLevel :: Sequencer -> System World ()
seqLevel s = return ()

newLevel :: Position -> InstrumentName ->  InstrumentName ->  System World ()
newLevel p@(Position pn) i1 i2 = do
  cmapM $ \(a@(MBoard ms m) :: Netitor) -> do
    mapM_ (\e -> e `modify` (\pp -> pp + p)) a
    return $ MBoard (fmap (\(Box (po,w,h),e) -> (Box (po + pn,w,h), e)) ms) m
  
moveStuffRandomly :: V2 CDouble -> Entity -> CDouble -> System World ()
moveStuffRandomly r e a = do
  {--op <- liftIO $ (\m n -> (\i -> signum i * a + i) <$> [n,m]) 
    <$> (randomDonutBox 360 500) 
    <*> (randomRIO (-1000,1000)) 
  let [o,p] = if length op == 2 then op else [0,0]--}
  (o,p) <- liftIO $ (,) <$> randomDonutBox 360 500 <*> randomDonutBox 360 500
  e `modify` (\(Box (_,x,y)) -> Box (r + V2 o p, x, y))
  e `set` (Velocity 0,Position (r + V2 o p))

getGrid :: Position -> (Int,Int)
getGrid (Position p1@(V2 x1 y1)) = (floor . negate $ (x1 / 1000) - 2, floor . negate $ (y1 / 1000) )

saveGridCell :: Position -> SystemT World IO ()
saveGridCell p =
  cmapM $ \g@(Grid is _) -> do
    let (gx,gy) = getGrid p
    sqs <- flip cfoldM [] $ \a (s :: (Sequencer,Entity)) -> return (s:a)
    case M.lookup gx is of
      Just ys -> case M.lookup gy ys of
                    Just ss -> return $ Grid (M.insert gx (M.insert gy (fst <$> sqs) ys) is) (gx,gy)
                    Nothing -> return g
      Nothing -> return g

checkGridCell :: Position -> SystemT World IO ()
checkGridCell p = 
  cmapM $ \g@(Grid is last@(lx,ly)) -> do
    let gxy@(gx,gy) = getGrid p
        moveJellys = cmapM $ \(MBoard ms t :: Netitor) -> do
          let gridPos p0 = p0 + (pure 1000 * ( fromIntegral <$> (V2 lx ly - V2 gx gy) ))
          cmap $ \case
            (Position p0, Box (_,w,h), Wall) -> (Position $ gridPos p0, Box (gridPos p0,w,h))
            (p,b,_) -> (p,b)
          newms <- flip mapM ms $ \(Box (p0,w,h),e) -> do
            notPlayer <- not <$> exists e (Proxy :: Proxy Player)
            when notPlayer $ modify e $ \(Position _, Box (_,w,h)) -> (Position $ gridPos p0, Box (gridPos p0,w,h))
            return (Box (gridPos p0,w,h), e)
          --liftIO $ writeFile "temp.txt" (show newms ++ "===" ++ show p ++ "===" ++ show last ++ "===" ++ show gxy)
          return $ MBoard newms t
          
    if last == gxy then return g else do    
      sqs <- flip cfoldM [] $ \a s -> return ((s:a) :: [((Sequencer, Entity),InstrumentName)])
      rsts <- flip cfoldM [] $ \a (SFXResources _ s _, e :: Entity, i :: InstrumentName) -> 
                return $ case getNote s of
                  Right _ -> (e,i):a 
                  Left _ -> a
      let emptyBoard i = fillBAll $ (\a -> [a]) $ fst $ head $ filter ((==i).snd) rsts
      let emptyBoards = (emptyBoard . snd) <$> sqs
      moveJellys
      if length is > 10 then return $ Grid mempty (0,0) else do
        case M.lookup gx is of
          Just ys -> case M.lookup gy ys of
                        Just (ss) -> mapM_ (\(e,s) -> set e s) (zip ((snd.fst) <$> sqs) ss) >> return (Grid is (gx,gy))
                        Nothing -> cmap (\(s :: Sequencer,i) -> emptyBoard i) >> return (Grid (M.insert gx (M.insert gy emptyBoards ys) is) (gx,gy))
          Nothing -> cmap (\(s :: Sequencer,i) -> emptyBoard i) >> return (Grid (M.insert gx (M.insert gy emptyBoards mempty) is) (gx,gy))

