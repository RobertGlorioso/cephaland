{-# LANGUAGE ViewPatterns #-}
module Ceph.Component.Levels where

import qualified Data.IntMap as M
import Ceph.Components
import Ceph.Physics.Box
import Apecs
import Linear
import System.Random
import Control.Monad

moveStuffRandomly :: V2 Float -> Entity -> Float -> System World ()
moveStuffRandomly r e a = do
  [o,p] <- liftIO $ (\m n -> (\m -> signum m * a + m) <$> [n,negate m]) <$> (head <$> randomDonutBox 1 50 30 ) <*> (randomRIO (-500,500)) 
  e `modify` (\(Box (_,x,y)) -> Box ( (r + V2 o p) , x, y))
  e `set` (Velocity 0,Position (r + V2 o p))

randomizeGridCell :: Position -> SystemT World IO ()
randomizeGridCell (Position p1@(V2 x1 y1)) = 
  cmapM_ $ \(Grid is) -> do
    let (floor -> gx) = (x1 / 1000) + (signum x1)
        (floor -> gy) = (y1 / 200) + (signum y1)
        updateGrid g = cmap $ \(Grid _) -> Grid g
        moveEnemyWalls =
          cmapM_ $ \case
            (Wall, Out, e) -> moveStuffRandomly p1 e 0
            (Enemy, Out, e) -> moveStuffRandomly p1 e 0
            (_,_,_) -> return ()
                                    
    if length is > 10 then updateGrid mempty else return ()
    case M.lookup gx is of
      Just ys -> if gy `elem` M.keys ys then return ()

                 
                 else updateGrid (M.insert gx (M.insert gy () ys) is) >> moveEnemyWalls
      Nothing ->  moveEnemyWalls >> updateGrid (M.insert gx mempty is)
