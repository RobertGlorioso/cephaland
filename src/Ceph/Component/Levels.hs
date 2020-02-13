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
    let (floor -> gx) = (x1 / 1500) + (signum x1)
        (floor -> gy) = (y1 / 400) + (signum y1)
        updateGrid g = cmap $ \(Grid _) -> Grid g
        moveEnemyWalls =
          cmapM_ $ \case
            (Wall1, Out, e) -> moveStuffRandomly p1 e 100
            --(Enemy, Out, e) -> moveStuffRandomly p1 e 1000
            (_,_,_) -> return ()
                                    
    if length is > 10 then updateGrid mempty else return ()
    case M.lookup gx is of
      Just ys -> if gy `elem` M.keys ys then return ()
                  else updateGrid (M.insert gx (M.insert gy () ys) is) >> moveEnemyWalls
      Nothing -> moveEnemyWalls >> updateGrid (M.insert gx mempty is)
