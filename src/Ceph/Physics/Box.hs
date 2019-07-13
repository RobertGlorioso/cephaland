{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Ceph.Physics.Box where

import Ceph.Util
import Ceph.Components
import Data.List hiding (find)
import Data.Vector (find)
import Data.Functor.Rep
import Data.Functor.Adjunction
import Data.Distributive
import System.Random
import Graphics.Gloss
import Control.Monad
import Apecs
import Linear


friction = 1.27

boxBound :: System World ()
boxBound = do
  --recursive version that finds a "neareast wall" for each moving, in-scope ent
  inScopeWall <- cfoldM (\a b@(_,_,_,s,_) -> if s == In then return (b:a) else return a ) [] :: System World [(Box,Angle,Wall,Scope,Entity)]
  cmapM_ $ wallBounce inScopeWall 
  return ()


data TouchedEdge = LeftEdge | RightEdge | TopEdge | BottomEdge | TopRightCorner | NotTouched | BottomRightCorner | BottomLeftCorner | TopLeftCorner deriving (Eq, Show, Ord)

aabb :: Box -> Box -> Bool
aabb (Box (V2 ax ay,w1,h1)) (Box (V2 bx by,w2,h2)) = all (<0)
  [ax - w1 - (bx + w2)
   , bx - w2 - (ax + w1)
   , ay - h1 - (by + h2)
   , by - h2 - (ay + h1) ]

rotate_box_ccw ::
  Box -> Angle -> (Position, Box) -> (Position, Box)
rotate_box_ccw (Box ((V2 ax ay), w1, h1)) (Angle alpha) pb@(Position (V2 bx by), (Box (_,w2,h2)))
  | alpha > (-0.1) && alpha < (0.1) = pb
  | True =
    let bx' = (bx - ax)
        by' = (by - ay)
        newpos = ( (V2 (bx' * cos alpha - by' * sin alpha) ( bx' * sin alpha + by' * cos alpha )))
    in (Position newpos , Box ( newpos , w2, h2))

rotate_box_cw ::
  Box -> (Position, Box) -> Angle -> (Position, Box)
rotate_box_cw (Box ((V2 ax ay), w1, h1)) pb@(Position (V2 bx by), (Box (_,w2,h2))) (Angle alpha)
  | alpha > (-0.1) && alpha < (0.1) = pb
  | True =
      let bx' = (bx - ax)
          by' = (by - ay)
          newpos = ( (V2 ax ay) + (V2 (bx' * cos alpha + by' * sin alpha) ( by' * cos alpha - bx' * sin alpha )))

      in (Position newpos , Box ( newpos , w2, h2))
                                                      
reflect_vel_box :: V2 Float -> V2 Float -> (Velocity,Position) -> (Velocity,Position)
reflect_vel_box a f (Velocity v,Position p) = let new_v =  reflect_v v (normal_v a)
  in ( Velocity $ new_v / f, Position $ p )
                                                      
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

playerMarker :: Position -> Velocity -> System World Entity
playerMarker p (Velocity (V2 vx vy)) = newEntity (( p
                        , 0 :: Velocity
                        , BodyPicture $ Pictures $ Line <$> [[(0,0),(vx,vy)],[(-1,0),(1,0)],[(0,-1),(0,1)]] 
                        , Box (0, 0.1, 0.1))
                        , Angle 0)

ang :: Angle -> V2 Float
ang (Angle m) = angle m

pos :: Float -> Float -> Position
pos a b = Position $ V2 a b

--wallBounce :: [(Box, Angle, Wall, Scope, Entity)] -> (c -> System World ())
wallBounce ::
  [(Box, Angle, Wall, Scope, Entity)]
  -> ((Box, Scope), SFXResources, Velocity, Position, Entity, Behavior,
      Actor)
  -> System World ()
  
wallBounce []  _  = return ()
wallBounce _ ((_, Out), _, _, _, _, _, _)  = return ()
wallBounce
  ((b, n, _, _, eb ):rest)
  c@((a,_),SFXResources _ j,_,p,e,h,Projectile)
  = do

  let new_a = (snd $ rotate_box_cw b  (p,a) n)
  if not $ touched new_a b
      then do
        (when (h == Sing) $ e `set` NoBehavior ) >> wallBounce rest c
      else do        
        get e >>= \case
          Bullet -> eb `modify` (\(SFXResources b _) -> (SFXResources b j)) >> e `set` ( Position $ pure 2e7 )
          Arrow -> eb `modify` (\(SFXResources b _) -> (SFXResources b j)) >> e `set` ( Plant ) 
wallBounce
  ((b, n, _, _, _):rest)
  c@((a,_),_,_,p,e,_,Weapon) = do
    let new_a = snd $ rotate_box_cw b (p,a) n
    if not $ touched new_a b then wallBounce rest c else do
      e `set` ()
wallBounce
  ((b, n, _, _, _):rest)
  c@((a,_),_,v@(Velocity vp),p,e,l,Player)
  = do
  let new_a = snd $ rotate_box_cw b (p,a) n -- the new player box that is obtained via rotating it by the wall's angle
  let em = edgeMeasures new_a b -- the distances to every boxes edge
  Gravity (V2 _ g)  <- get global 
  let new_l = if (norm vp > (-8 * g)) || l == Swinging  then l else Plant --sets the player behavior for being planted
  if not $ touched new_a b then wallBounce rest c else do    
     case minni (abs <$> em) [RightEdge, LeftEdge, TopEdge, BottomEdge] of 
        (d,TopEdge) -> unless ( new_l `elem` [Carry,Plant] ) $ e `set` (new_l,reflect_vel_box (ang n) friction (v,p)) 
        (d,BottomEdge) -> unless ( new_l `elem` [Carry,Plant] ) $ e `set` (new_l,reflect_vel_box (ang n) friction (v,p)) 
        (d,RightEdge) -> unless ( l `elem` [Carry,Plant] ) $ e `set` (new_l,reflect_vel_box (ang n) friction (negate v,p )) 
        (d,LeftEdge) -> unless ( l `elem` [Carry,Plant] ) $ e `set` (new_l,reflect_vel_box (ang n) friction (negate v,p )) 
wallBounce
  ((b, n, _, _, _):rest)
  c@((a,_),_,v,p,e,l,Enemy)
  = do
    let new_a = (snd $ rotate_box_cw b (p,a) n)
    let em = edgeMeasures new_a b
    if not $ touched new_a b then (when (l `elem` [Plant,Sing]) $ e `set` NoBehavior ) >> wallBounce rest c else do
      case minni (abs <$> em) [RightEdge, LeftEdge, TopEdge, BottomEdge] of -- edge checking to give the correct normal 
        (d,TopEdge) -> unless ( l `elem` [Carry,Plant] ) $ e `set` (reflect_vel_box (ang n) friction (v,p + pos 0 d)) 
        (d,BottomEdge) -> unless ( l `elem` [Carry,Plant] ) $ e `set` (reflect_vel_box (ang n) friction (v,p - pos 0 d)) 
        (d,RightEdge) -> unless ( l `elem` [Carry,Plant] ) $ e `set` (reflect_vel_box (ang n) friction (negate v,p - pos d 0)) 
        (d,LeftEdge) -> unless ( l `elem` [Carry,Plant] ) $ e `set` (reflect_vel_box (ang n) friction (negate v,p + pos d 0)) 
wallBounce _ _ = return ()    
--}


edgeMeasures :: Box -> Box -> [Float]
edgeMeasures (Box (V2 bx by, w2, h2)) (Box (V2 ax ay, w1, h1)) =
      [ ax - w1 - (bx + w2)
      , bx - w2 - (ax + w1)
      , ay - h1 - (by + h2)
      , by - h2 - (ay + h1) ]

touched :: Box -> Box -> Bool
touched a b = all (<0) $ edgeMeasures a b
  
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


--other way of performing the box collision
--a fold?


foldBoxBound :: System World ()
foldBoxBound = do
  g :: Gravity <- get global
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
  

loopBoxBound :: Gravity -> System World ()
loopBoxBound g = cmapIfM_

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
            when (a `Data.List.elem` [Enemy, Projectile]) $ e `set` Sing
            when (a == Projectile) $ e `set` Position 2e7
        )
  )
