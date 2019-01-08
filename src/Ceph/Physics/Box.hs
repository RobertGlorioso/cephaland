{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Ceph.Physics.Box where

import Ceph.Util
import Ceph.Components
import Data.List
import System.Random
import Control.Monad
import Apecs
import Apecs.System
import Linear
import qualified SDL.Mixer as M

data Edge = LeftEdge | RightEdge | TopEdge | BottomEdge | TopRightCorner | BottomRightCorner | BottomLeftCorner | TopLeftCorner deriving (Eq, Show, Ord)

aabb :: Box -> Box -> Bool
aabb (Box (V2 ax ay,w1,h1)) (Box (V2 bx by,w2,h2)) = all (<0)
  [ax - w1 - (bx + w2)
   , bx - w2 - (ax + w1)
   , ay - h1 - (by + h2)
   , by - h2 - (ay + h1) ]

minni :: Ord a => [a] -> [b] -> (a,b)
minni a b = minimumBy (\c d -> compare (fst c) (fst d)) $ zip a b

box :: V2 Float -> Float -> Float -> Box
box (V2 x y) w h = Box ((V2 x y), w, h)

randomDonutBox :: Int -> Float -> Float -> IO [Float]
randomDonutBox l holeRadius diam = (fmap.fmap) (\a ->  signum a * holeRadius + a) $ replicateM l $ randomRIO (negate diam,diam)
  
boxBound :: System World ()
boxBound = do
  
  inScopeWall <- filter (\(_,a,i) -> a == Wall && i == In) <$> (getAll :: System World [(Box,Actor,Scope)])
  cmapM_ $ wallBounce inScopeWall 
  --other ways of performing the box collisions

  --cfoldM ( \_ ( b@(Box (c, w, h)), _ :: Not Wall, e) -> cmapM_ $ wallBounceE e b) ()
  --concurrently $ [ bounce c1 c2 |  c2@(b2,_,_) <- inScopeWalls, c1@(b1,_,_) <- inScopeMovingObjs, aabb b1 b2]

  --cfoldM ( \_ ( b@(Box (c, w, h)), _ :: Not Wall, e) -> cmapM_ $ wallBounceE e b) () --}
  
edgeMeasures :: Box -> Box -> [Float]
edgeMeasures (Box (V2 ax ay, w1, h1)) (Box (V2 bx by, w2, h2)) =
      [ ax - w1 - (bx + w2)
      , bx - w2 - (ax + w1)
      , ay - h1 - (by + h2)
      , by - h2 - (ay + h1) ]

touched :: Box -> Box -> Bool
touched a b = all (<0) $ edgeMeasures a b

collideProc :: Box -> Box -> Velocity -> Position -> Entity -> System World ()
collideProc b1 b2 (Velocity v@(V2 v1 v2)) (Position p) otherEnt = do
      Gravity (V2 _ g) <- get global
      
      let friction = 1.90
      case minni (abs <$> edgeMeasures b1 b2) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
        (d,TopEdge) -> otherEnt `modify` (\(b,_,_) -> (if norm v < (-10)*g then Plant else b, Position $ p + V2 v1 (abs v2 + d), Velocity (V2 v1 (abs v2) / friction)))
        (d,BottomEdge) -> otherEnt `set` (Position $ p + V2 v1 ( negate . abs $ v2 - d), Velocity (V2 v1 (negate . abs $ v2) / friction))
        (d,RightEdge) -> otherEnt `set` (Position $ p + V2 (abs v1 + d) v2, Velocity (V2 (abs v1) v2 / friction))
        (d,LeftEdge) -> otherEnt `set` (Position $ p + V2 ( negate . abs $ v1 - d) v2, Velocity (V2 (negate . abs $ v1) v2 / friction))

wallBounce ::
  [(Box, Actor, Scope)]
  -> (Box, Velocity, Position, Entity, Behavior, Actor)
  -> SystemT World IO ()
wallBounce [] _ = return ()
wallBounce
  ((b, _, _):rest)
  c@(a,v,p,e,_,Projectile) = do
    if not (touched a b) then wallBounce rest c else do
      get e >>= \case
        Bullet -> e `set` (Sing, Position $ pure 2e7 )
        Arrow -> e `set` (Sing)
wallBounce
  ((b, _, _):rest)
  c@(a,v,p,e,l,Player) = do
    if touched a b then wallBounce rest c else do
        unless ( l `elem` [Carry,Plant] ) $ collideProc a b v p e
wallBounce
  ((b, _, _):rest)
  c@(a,v,p,e,_,Enemy) = do
    if touched a b then wallBounce rest c else do
        e `set` Sing >> collideProc a b v p e
wallBounce _ _ = return ()    
