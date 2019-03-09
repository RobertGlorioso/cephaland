{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Ceph.Physics.Box where

import Ceph.Components
import Data.List
import System.Random
import Control.Monad
import Apecs
import Linear
import Data.Ord
import Data.List

data TouchedEdge = LeftEdge | RightEdge | TopEdge | BottomEdge | TopRightCorner | NotTouched | BottomRightCorner | BottomLeftCorner | TopLeftCorner deriving (Eq, Show, Ord)

aabb :: Box -> Box -> Bool
aabb (Box (V2 ax ay,w1,h1)) (Box (V2 bx by,w2,h2)) = all (<0)
  [ax - w1 - (bx + w2)
   , bx - w2 - (ax + w1)
   , ay - h1 - (by + h2)
   , by - h2 - (ay + h1) ]

rotate_box_ccw ::
  Box -> Angle -> (Position, Box) -> (Position, Box)
rotate_box_ccw (Box ((V2 ax ay), w1, h1)) (Angle alpha) pb@(Position _, (Box ((V2 bx by),w2,h2)))
  | alpha > -1 && alpha < 1 = pb
  | True =
    let bx' = (bx - ax)
        by' = (by - ay)
        newpos = ( (V2 (bx' * cos alpha - by' * sin alpha) ( bx' * sin alpha + by' * cos alpha )))
    in (Position newpos , Box ( newpos , w2, h2))

rotate_box_cw ::
  Box -> Angle -> (Position, Box) -> (Position, Box)
rotate_box_cw (Box ((V2 ax ay), w1, h1)) (Angle alpha) pb@(Position _, (Box ((V2 bx by),w2,h2)))
  | alpha > -1 && alpha < 1 = pb
  | True =
      let bx' = (bx - ax)
          by' = (by - ay)
          newpos = ( (V2 ax ay) + (V2 (bx' * cos alpha + by' * sin alpha) ( by' * cos alpha - bx' * sin alpha )))

      in (Position newpos , Box ( newpos , w2, h2))
                                                      
reflect_vel_box :: Box -> V2 Float -> V2 Float -> (Velocity,Position) -> (Velocity,Position)
reflect_vel_box (Box ((V2 ax ay), w1, h1)) a f (Velocity v,Position p) = let new_v =  reflect_v v (normal_v a) / f
  in ( Velocity $ new_v, Position $ p + new_v )
                                                      
reflect_v :: V2 Float -> V2 Float -> V2 Float
reflect_v v1 n = v1 - (pure (2 * (v1 `dot` n)) * n)

normal_v :: V2 Float -> V2 Float
normal_v (V2 x y) = (V2 (negate y) x)


minni :: Ord a => [a] -> [b] -> (a,b)
minni a b = minimumBy (\c d -> compare (fst c) (fst d)) $ zip a b

minni' :: [Float] -> [TouchedEdge] -> (Float,TouchedEdge)
minni' a b = let c = maximumBy (\c d -> compare (fst c) (fst d)) $ zip a b in if (fst c) < 0 then c else (0,NotTouched) 

box :: V2 Float -> Float -> Float -> Box
box (V2 x y) w h = Box ((V2 x y), w, h)

randomDonutBox :: Int -> Float -> Float -> IO [Float]
randomDonutBox l holeRadius diam = (fmap.fmap) (\a ->  signum a * holeRadius + a) $ replicateM l $ randomRIO (negate diam,diam)
  
boxBound :: System World ()
boxBound = do
  --simple recursive version that finds a "neareast wall" for each moving, in-scope ent

  --this is faster than the fold version directly below
  g :: Gravity <- get global
  inScopeWall <- cfoldM (\a b@(_,_,_,s) -> if s == In then return (b:a) else return a ) [] :: System World [(Box,Angle,Wall,Scope)]
  --liftIO . print $ length inScopeWall
  cmapM_ $ wallBounce inScopeWall 
  return ()
  --other way of performing the box collision
  {--a fold?
  flip cfoldM_ () $ \() -> \case
    (_, _, Wall, _) -> return ()
    (_, Out, _, _) -> return ()
    (b_, In, Player, e) -> do
      flip cfoldM_ () $ \() -> \case
        (_, Out,_) -> return ()
        (Wall1, In, c) -> when (aabb b_ c) $ ( e `modify` collideProc' c (Just g))
    (b_, In, Enemy, e) -> do
      flip cfoldM_ () $ \() -> \case
        (_, Out,_) -> return ()
        (Wall1, In, c) -> when (aabb b_ c) $
          e `modify` collideProc' c (Just g) >> e `set` Sing
    (b_, In, Projectile, e) -> do
      flip cfoldM_ () $ \() -> \case
        (_, Out,_) -> return ()
        (Wall1, In, c) -> when (aabb b_ c) $ e `set` (Position (pure 2e7), Sing)
    (b_, In, Weapon, e) -> do
      flip cfoldM_ () $ \() -> \case
        (_, Out,_) -> return ()
        (Wall1, In, c) -> when (aabb b_ c) (e `modify` collideProc' c (Just g) >> e `set` Sing)
          
              

  --similar to the first but with recursion factored out to loops
  

  cmapIfM_
    (\case
        (Wall,_) -> False
        (_,Out) -> False
        _ -> True
    )
    (\( b_, a, e) -> do
        conceIfM_
          (\case
              (_, Out, c) -> False
              (a_, In, c) -> a_ /= Wall && aabb b_ c
          )
          (\b -> do
              when (a == Player) $ e `modify` collideProc' b (Just g)
              when (a `elem` [Enemy, Projectile]) $ e `set` Sing
              when (a == Projectile) $ e `set` Position 2e7
          )
    ) --}

edgeMeasures :: Box -> Box -> [Float]
edgeMeasures (Box (V2 bx by, w2, h2)) (Box (V2 ax ay, w1, h1)) =
      [ ax - w1 - (bx + w2)
      , bx - w2 - (ax + w1)
      , ay - h1 - (by + h2)
      , by - h2 - (ay + h1) ]

touched :: Box -> Box -> Bool
touched a b = all (<0) $ edgeMeasures a b

friction = 1.03
 
collideProc' :: Box -> Maybe Gravity -> (Box, Velocity, Position, Actor, Behavior) -> (Box, Velocity, Position, Actor, Behavior)
collideProc' b2 g c@(b1,  Velocity v@(V2 v1 v2), Position p, a, Plant) = c
collideProc' b2 g c@(b1,  Velocity v@(V2 v1 v2), Position p, a, Carry) = c
collideProc' b2 g (b1,  Velocity v@(V2 v1 v2), Position p, a, l) = do
      case minni' (edgeMeasures b1 b2) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
        (_,NotTouched) -> (b1, Velocity v, Position p, a, l)
        (d,TopEdge) -> (b1
                        , Velocity (V2 v1 (abs v2) / friction)
                        , Position $ p + V2 v1 (abs v2 + d)
                        , a
                        , maybe l (\(Gravity (V2 _ jg)) -> if norm v < (-10)*jg then Plant else l) g )
        (d,BottomEdge) -> (b1
                          , Velocity (V2 v1 (negate . abs $ v2) / friction)
                          , Position $ p + V2 v1 (negate . abs $ v2 - d)
                          , a
                          , l)
        (d,RightEdge) -> (b1
                         , Velocity (V2 (abs v1) v2 / friction)
                         , Position $ p + V2 (abs v1 + d) v2
                         , a
                         , l)
        (d,LeftEdge) -> (b1
                        , Velocity (V2 (negate . abs $ v1) v2 / friction)
                        , Position $ p + V2 ( negate . abs $ v1 - d) v2
                        , a
                        , l)

collideProc :: (Angle, Box) -> (Box, Velocity,  Position, Entity) -> System World ()
collideProc (Angle alpha,b1) (b2, (Velocity v@(V2 v1 v2)), (Position p), otherEnt) = do
  let newb1 = (snd $ rotate_box_cw b2 (Angle alpha) (Position p,b1))
  otherEnt `modify` reflect_vel_box b1 (angle alpha) friction

wallGrab :: [(Box, Angle, Wall, Scope, Entity)]
         -> ( Target  ) -> System World ()
wallGrab [] _ = return ()
wallGrab ((b,n,_,_,eb):rest) (Target t) = do
  
  let new_a = (snd $ rotate_box_cw b n (Position t,Box (t, 0.5, 0.5)))
  ls <- cfoldM (\a b -> return (b:a)) [] :: System World [(Linked, Entity)]
  let (Linked prv tar,e) = maximumBy (comparing fst) ls
  if not $ touched new_a b
     then do
        wallGrab rest (Target t)
      else do
        e `set` Linked prv eb
  
wallBounce ::
  [(Box, Angle, Wall, Scope)]
  -> (Box, Scope, Velocity, Position, Entity, Behavior, Actor)
  -> SystemT World IO ()
wallBounce [] _ = return ()
wallBounce _ (_, Out, _, _, _, _, _) = return ()
wallBounce
  ((b, n, _, _):rest)
  c@(a,_,v,p,e,h,Projectile) = do
    let new_a = (snd $ rotate_box_cw b n (p,a))
    if not $ touched new_a b
      then do
        (when (h == Sing) $ e `set` NoBehavior ) >> wallBounce rest c
      else do
      get e >>= \case
        Bullet -> e `set` (Sing, Position $ pure 2e7 )
        Arrow -> e `set` (Sing)
wallBounce
  ((b, n, _, _):rest)
  c@(a,_,v,p,e,_,Weapon) = do
    let new_a = snd $ rotate_box_cw b n (p,a)
    if not $ touched new_a b then wallBounce rest c else do
      e `set` (Sing)
wallBounce
  ((b, n, _, _):rest)
  c@(a,_,v,p,e,l,Player)
  = do
  let new_a = snd $ rotate_box_cw b n (p,a)
  
  if not $ touched new_a b then wallBounce rest c else do
    case minni (abs <$> edgeMeasures new_a b) [RightEdge, LeftEdge, TopEdge, BottomEdge] of -- edge checking to give the correct normal 
      (d,TopEdge) -> unless ( l `elem` [Carry,Plant] ) $ collideProc (n, b) (new_a, v, p, e)
      (d,BottomEdge) -> unless ( l `elem` [Carry,Plant] ) $ collideProc (n, b) (new_a, v, p, e)
      (d,RightEdge) -> unless ( l `elem` [Carry,Plant] ) $ collideProc (n + (Angle $ pi/2) , b) (new_a, v, p, e)
      (d,LeftEdge) -> unless ( l `elem` [Carry,Plant] ) $ collideProc (n + (Angle $ pi/2), b) (new_a, v, p, e)
    
wallBounce
  ((b, n, _, _):rest)
  c@(a,_,v,p,e,h,Enemy)
  = do
    let new_a = (snd $ rotate_box_cw b n (p,a))
    if not $ touched new_a b then (when (h `elem` [Plant,Sing]) $ e `set` NoBehavior ) >> wallBounce rest c else do
        e `set` Sing >> collideProc (n,b) (new_a, v, p, e)
wallBounce _ _ = return ()    
