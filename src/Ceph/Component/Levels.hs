{-# LANGUAGE ViewPatterns #-}
module Ceph.Component.Levels where

import qualified Data.IntMap as M
import Ceph.Components
import Ceph.Physics.Box
import Apecs
import Linear
import Foreign.C.Types

moveStuffRandomly :: V2 CDouble -> Entity -> CDouble -> System World ()
moveStuffRandomly r e a = do
  {--op <- liftIO $ (\m n -> (\i -> signum i * a + i) <$> [n,m]) 
    <$> (randomDonutBox 360 500) 
    <*> (randomRIO (-1000,1000)) 
  let [o,p] = if length op == 2 then op else [0,0]--}
  (o,p) <- liftIO $ (,) <$> randomDonutBox 360 500 <*> randomDonutBox 360 500
  e `modify` (\(Box (_,x,y)) -> Box ( r + V2 o p, x, y))
  e `set` (Velocity 0,Position (r + V2 o p))

randomizeGridCell :: Position -> SystemT World IO ()
randomizeGridCell (Position p1@(V2 x1 y1)) = 
  cmapM_ $ \(Grid is) -> do
    let (floor -> gx) = (x1 / 500) + (signum x1)
        (floor -> gy) = (y1 / 500) + (signum y1)
        updateGrid g = cmap $ \(Grid _) -> Grid g
        --moveEnemyWalls = 
          --cmapM_ $ \case
            --(Wall1, Out, e) -> moveStuffRandomly p1 e 100
            --(Enemy, Out, e) -> moveStuffRandomly p1 e 1000
            --_ -> return ()
    sqs <- flip cfoldM [] $ \a s -> return (s:a)                             
    if length is > 10 then updateGrid mempty else return ()
    case M.lookup gx is of
      Just ys -> case M.lookup gy ys of
                    Just (ss) -> return () -- mapM_ (\e -> cmap (\(SBoard _ _ _ _ :: Sequencer) -> e)) ss
                    Nothing -> updateGrid (M.insert gx (M.insert gy sqs ys) is)
      Nothing ->  updateGrid (M.insert gx (M.insert gy sqs mempty) is)
