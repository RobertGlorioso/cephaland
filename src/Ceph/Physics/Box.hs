{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Ceph.Physics.Box where

import Ceph.Util
import Ceph.Components
import Data.List
import Foreign.C.Types
import System.Random
import Control.Monad
import Apecs
import Linear hiding (angle)
import qualified Linear

friction :: CDouble
friction = 1.08

boxBound :: System World ()
boxBound = do
  --recursive version that finds a "neareast wall" for each moving, in-scope ent
  inScopeWall <- cfoldM (\a b@(_,_,_,s,_) -> if s == In then return (b:a) else return a ) [] :: System World [(Box,Angle,Wall,Scope,Entity)]
  cmapM_ $ wallBounce inScopeWall

data Edge = LeftEdge 
  | RightEdge 
  | TopEdge 
  | BottomEdge deriving (Eq, Show, Ord)

--checks that two boxes are touching
aabb :: Box -> Box -> Bool
aabb b1 b2 = all (<0) $ edgeMeasures b1 b2

--checks that the first box is below the second
isBelow :: Box -> Box -> Bool
isBelow (Box (V2 _ ay,_,h1)) (Box (V2 _ by,_,h2)) = by + h2 > ay - h1

--calculates the distance between two boxes edges. if all are negative they are touching 
edgeMeasures :: Box -> Box -> [CDouble]
edgeMeasures (Box (V2 bx by, w2, h2)) (Box (V2 ax ay, w1, h1)) =
  [ ax - w1 - (bx + w2)
  , bx - w2 - (ax + w1)
  , ay - h1 - (by + h2)
  , by - h2 - (ay + h1) ]

updateAM :: Box -> Box -> Angle -> AngularMomentum -> AngularMomentum
updateAM b1 b2 n m = m

rotate_box_cw :: Box -> (Box, Angle) -> Box
rotate_box_cw (Box (V2 ax ay, _, _)) (b@(Box (V2 bx by, w2, h2)), Angle n)
  | n > (-0.1) && n < (0.1) = b
  | True =
      let bx' = bx - ax
          by' = by - ay
          newpos = (V2 ax ay) + V2 (bx' * cos n + by' * sin n) (by' * cos n - bx' * sin n)
      in Box (newpos, w2, h2)

rotate_pos_cw :: Position -> (Box,Angle) -> Position
rotate_pos_cw p0@(Position (V2 ax ay)) (Box (V2 bx by, _, _),Angle n)
  | n > (-0.1) && n < (0.1) = p0
  | True =
      let bx' = bx - ax
          by' = by - ay
          newpos = (V2 ax ay) + V2 (bx' * cos n + by' * sin n) (by' * cos n - bx' * sin n)
      in Position newpos

rotate_cw :: Position -> (Box,Angle) -> V2 CDouble
rotate_cw (Position p0@(V2 bx by)) (Box ((V2 ax ay), _, _),Angle n)
  | n > (-0.1) && n < (0.1) = p0
  | True =
      let bx' = bx - ax
          by' = by - ay
      in (V2 ax ay) + V2 (bx' * cos n + by' * sin n) (by' * cos n - bx' * sin n)

--reflecting a velocity vector. the first argument is the wall's angle as a vector
reflect_vel :: V2 CDouble -> (Velocity, Position) -> (Velocity, Position)
reflect_vel a (Velocity v, Position p) = let new_v =  reflect v (normal_v a)
  in ( Velocity $ new_v / pure friction, Position p)
  where 
    reflect :: V2 CDouble -> V2 CDouble -> V2 CDouble
    reflect v1 vn = v1 - (pure (2 * (v1 `dot` vn)) * vn)

--the normal to a vector
--used to reflect a moving object's velocity when it collides 
normal_v :: V2 CDouble -> V2 CDouble
normal_v (V2 x y) = (V2 (negate y) x)

minni :: Ord a => [a] -> [b] -> (a,b)
minni a b = minimumBy (\c d -> compare (fst c) (fst d)) $ zip a b

box :: V2 CDouble -> CDouble -> CDouble -> Box
box (V2 x y) w h = Box ((V2 x y), w, h)

randomDonutBox :: CDouble -> CDouble -> IO CDouble
randomDonutBox holeRadius diam = (\a ->  signum a * holeRadius + a) <$> randomRIO (negate diam,diam)

angle2vec :: Angle -> V2 CDouble
angle2vec (Angle (CDouble m)) = CDouble <$> Linear.angle m
 
position :: CDouble -> CDouble -> Position
position a b = Position $ V2 a b

squallBounce :: (Box, Velocity, Player) -> (Box, Velocity, Projectile) -> (Velocity)
squallBounce (bp, vp, _) (bs, vs, Squall) = if aabb bp bs then (Velocity (pure 0.9) * vp + (Velocity (pure 0.1) * vs)) else vs
squallBounce _ (_, v, _)  = v

wallBounce ::
  [(Box, Angle, Wall, Scope, Entity)]
  -> ((Box, Scope), SFXResources, Phys, Entity, Behavior, Actor)
  -> System World ()
wallBounce []  _  = return ()
wallBounce _ ((_, Out), _, _, _, _, _)  = return ()
wallBounce
  ((b, n, _, _, eb):rest)
  c@((a,_),s,(v,p,_,_),e,h,Projectile)
  = do
  let new_a = rotate_box_cw b (a,n)
  if not $ aabb new_a b
    then do
      (when (h == Sing) $ e `set` NoBehavior ) >> wallBounce rest c
    else do
      get e >>= \case
        Bullet -> eb `set` (s, Sing) >> e `set` ( Position $ pure 2e7 )
        Arrow -> eb `set` (s, Sing) >> e `set` ( Plant )
        Squall -> do
          let chkPos i = rotate_box_cw b ((\(Velocity vel) (Box (p,x,y)) -> Box ((normalize vel / pure i) + p, x, y)) v a, n) -- the new player box that is obtained via rotating it by the wall's angle
          let em = edgeMeasures (chkPos 1) b -- the distances to the wall's edge
          if not $ any ((`aabb` b ). chkPos) [0.3,0.5..2.0] then wallBounce rest c else do
            eb `set` Sing
            case minni (abs <$> em) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
              (d,TopEdge) ->  e `set` reflect_vel (angle2vec n) (v, p) -- - position 0 d)
              (d,BottomEdge) ->  e `set` reflect_vel (angle2vec n) (v, p) -- + position 0 d)
              (d,RightEdge) ->  e `set` reflect_vel (angle2vec n) (negate v, p) -- - position d 0)
              (d,LeftEdge) ->  e `set` reflect_vel (angle2vec n) (negate v, p) -- + position d 0)
wallBounce
  ((b, n, _, _, _):rest)
  c@((a,_),_,(_,p,_,_),e,_,Weapon) = do
    let new_a = rotate_box_cw b (a,n)
    if not $ aabb new_a b then wallBounce rest c else do
      e `set` ()
wallBounce
  ((b, n, Floor, _, eb):rest)
  c@((a,_),_,(v@(Velocity (V2 vx vy)),p,_,_),e,_,Player)
  = do
    let chkPos i = rotate_box_cw b ((\(Velocity vel) (Box (o,x,y)) -> Box ((normalize vel / pure i) + o, x, y)) v a, n) -- the new player box that is obtained via rotating it by the wall's angle
    let em = edgeMeasures (chkPos 1) b 
    if not $ any ((`aabb` b ). chkPos) [0.3,0.5..2.0] then wallBounce rest c else do
      eb `set` Sing
      case minni (abs <$> em) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
        (d,TopEdge) ->  e `set` reflect_vel (angle2vec n) (Velocity $ V2 vx $ 0.5 * vy, (rotate_pos_cw (p - position 0 d) (a,n)))
        (d,BottomEdge) ->  e `set` reflect_vel (angle2vec n) (v, rotate_pos_cw (p + position 0 d) (a,n))
        (d,RightEdge) ->  e `set` reflect_vel (angle2vec n) (negate v, (rotate_pos_cw (p - position d 0) (a,n)))
        (d,LeftEdge) ->  e `set` reflect_vel (angle2vec n) (negate v, rotate_pos_cw (p + position d 0) (a,n))
wallBounce
  ((b, n, OneWayWall, _, _):rest)
  c@((a,_),_,(v,p,_,_),e,_,Player)
  = do
  let chkPos i = rotate_box_cw b ((\(Velocity vel) (Box (p,x,y)) -> Box ((normalize vel / pure i) + p, x, y)) v a, n) -- the new player box that is obtained via rotating it by the wall's angle
  let ems i = edgeMeasures (chkPos i) b
  if (not $ any ((`aabb` b ). chkPos) [0.3,0.5..2.0] ) then wallBounce rest c else do
    case (\i -> minni (abs <$> ems i) [RightEdge, LeftEdge, TopEdge, BottomEdge]) <$> [0.5,2.0] of
      [(d1,TopEdge),(d2,TopEdge)] -> 
        if d1 > d2 then set e $ reflect_vel (angle2vec n) (v, p) else  wallBounce rest c
      _ -> wallBounce rest c
wallBounce
  ((b, n, _, _, eb):rest)
  c@((a,_),_,(v,p,_,_),e,_,Player)
  = do
    let chkPos i = rotate_box_cw b ((\(Velocity vel) (Box (p,x,y)) -> Box ((normalize vel / pure i) + p, x, y)) v a, n) -- the new player box that is obtained via rotating it by the wall's angle
    let em = edgeMeasures (chkPos 1) b -- the distances to this box's edge
    if not $ any ((`aabb` b ). chkPos) [0.3,0.5..2.0] then wallBounce rest c else do
      eb `set` Sing
      case minni (abs <$> em) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
        (d,TopEdge) ->  e `set` reflect_vel (angle2vec n) (v, (rotate_pos_cw (p - position 0 d) (a,n)))
        (d,BottomEdge) ->  e `set` reflect_vel (angle2vec n) (v, rotate_pos_cw (p + position 0 d) (a,n))
        (d,RightEdge) ->  e `set` reflect_vel (angle2vec n) (negate v, (rotate_pos_cw (p - position d 0) (a,n)))
        (d,LeftEdge) ->  e `set` reflect_vel (angle2vec n) (negate v, rotate_pos_cw (p + position d 0) (a,n))
wallBounce
  ((b, n, wl, _, eb):rest)
  c@((a,_),sfx1@(SFXResources _ s _),(v,p,_,_),e,_,Enemy)
  = do
    let chkPos i = rotate_box_cw b ((\(Velocity vel) (Box (p,x,y)) -> Box ((normalize vel / pure i) + p, x, y)) v a, n) -- the new player box that is obtained via rotating it by the wall's angle
    let em = edgeMeasures (chkPos 1) b
    if not $ any ((`aabb` b ). chkPos) [0.5,2.0] then wallBounce rest c else do
      get global >>= \(BoardControl _ bdlk) -> when ( bdlk == Unlocked ) 
        (eb `modify` 
          (\sfx2@(SFXResources _ s2 _) -> 
            if getInst s == getInst s2 
            then sfx1
            else sfx2
          )
        )
      eb `set` Sing
      case minni (abs <$> em) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
        (d,TopEdge) ->  e `set` reflect_vel (angle2vec n) (v, (rotate_pos_cw (p - position 0 d) (a,n)))
        (d,BottomEdge) ->  e `set` reflect_vel (angle2vec n) (v, rotate_pos_cw (p + position 0 d) (a,n))
        (d,RightEdge) ->  e `set` reflect_vel (angle2vec n) (negate v, (rotate_pos_cw (p - position d 0) (a,n)))
        (d,LeftEdge) ->  e `set` reflect_vel (angle2vec n) (negate v, rotate_pos_cw (p + position d 0) (a,n))
wallBounce _ _ = return ()

{--
collideProc' :: Box -> Maybe Gravity -> (Box, Velocity, Position, Actor, Behavior) -> (Box, Velocity, Position, Actor, Behavior)
collideProc' b2 g c@(_,_,_,_, Plant) = c
collideProc' b2 g c@(_,_,_,_, Carry) = c
collideProc' b2 g (b1,  Velocity v@(V2 v1 v2), Position p, a, l) = do
      case minni' (edgeMeasures b1 b2) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
        (_,Notaabb) -> (b1, Velocity v, Position p, a, l)
        (d,TopEdge) ->  (b1
                        , Velocity (V2 v1 (abs v2) / pure friction)
                        , Position $ p + V2 v1 (abs v2 + d)
                        , a
                        , maybe l (\(Gravity (V2 _ jg)) -> if norm v < (-10)*jg then Plant else l) g )
        (d,BottomEdge) -> (b1
                          , Velocity (V2 v1 (negate . abs $ v2) / pure friction)
                          , Position $ p + V2 v1 (negate . abs $ v2 - d)
                          , a
                          , l)
        (d,RightEdge) -> (b1
                         , Velocity (V2 (abs v1) v2 / pure friction)
                         , Position $ p + V2 (abs v1 + d) v2
                         , a
                         , l)
        (d,LeftEdge) -> (b1
                        , Velocity (V2 (negate . abs $ v1) v2 / pure friction)
                        , Position $ p + V2 ( negate . abs $ v1 - d) v2
                        , a
                        , l)




--other way of performing the box collision


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
--}