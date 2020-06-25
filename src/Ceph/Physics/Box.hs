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
friction = 1.23

bounce :: (CDouble,CDouble) -> (Box, Velocity, Entity) -> (Box, Velocity, Entity) -> System World ()
bounce (a,b) (bp@(Box (bppos,_,_)), vp, ep) (bs@(Box (bspos,_,_)), vs, eo) =
  if aabb bp bs 
    then do 
      ep `set` (Velocity (pure b) * vp + (Velocity (pure a) * vs), Position (bppos + normalize (bppos - bspos)))
      eo `set` (Velocity (pure a) * vp + (Velocity (pure b) * vs), Position (bspos - normalize (bppos - bspos)))
    else return ()

shove :: (Box, Entity) -> (Box, Entity) -> System World ()
shove (bp@(Box (bppos,_,_)), ep) (bs@(Box (bspos,_,_)), eo) =
  if aabb bp bs 
    then do 
      ep `set` (Position (bppos + normalize (bppos - bspos)))
      eo `set` (Position (bspos - normalize (bppos - bspos)))
    else return ()

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
 -- | n > (-0.1) && n < (0.1) = p0
  | True =
      let bx' = bx - ax
          by' = by - ay
          newpos = (V2 ax ay) + V2 (bx' * cos n + by' * sin n) (by' * cos n - bx' * sin n)
      in Position newpos

rotate_cw :: Position -> (Box,Angle) -> V2 CDouble
rotate_cw (Position p0@(V2 bx by)) (Box ((V2 ax ay), _, _), Angle n)
  | n > (-0.1) && n < (0.1) = p0
  | True =
      let bx' = bx - ax
          by' = by - ay
      in (V2 ax ay) + V2 (bx' * cos n + by' * sin n) (by' * cos n - bx' * sin n)

--reflecting a velocity vector. the first argument is the wall's angle as a vector
reflect_vel :: Edge -> CDouble -> Angle -> Box -> (Velocity, Position) -> (Velocity, Position)
reflect_vel c d n a (Velocity v, p) = let new_v =  Velocity $ reflect v (normal_v $ angle2vec n) / pure friction
  in case c of
    TopEdge -> ( new_v , rotate_pos_cw (p - position 0 d) (a,n) )
    BottomEdge -> ( new_v , rotate_pos_cw (p + position 0 d) (a,n) )
    RightEdge -> ( negate new_v , rotate_pos_cw (p - position d 0) (a,n) )
    LeftEdge -> ( negate new_v , rotate_pos_cw (p + position d 0) (a,n) )

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
        --Bullet -> eb `set` (s, Sing) >> e `set` ( Position $ pure 2e7 )
        --Arrow -> eb `set` (s, Sing) >> e `set` ( Plant )
        Squall -> do
          let chkPos i = rotate_box_cw b ((\(Velocity vel) (Box (p,x,y)) -> Box ((normalize vel / pure i) + p, x, y)) v a, n) -- the new player box that is obtained via rotating it by the wall's angle
          let em = edgeMeasures (chkPos 1) b -- the distances to the wall's edge
          if not $ any ((`aabb` b ). chkPos) [1.0] then wallBounce rest c else do
            --eb `set` Sing
            case minni (abs <$> em) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
              (d,g) -> e `set` reflect_vel g d n a (v, p)
        _ -> return ()
wallBounce
  ((b, n, _, _, _):rest)
  c@((a,_),_,(_,p,_,_),e,_,Weapon) = do
    let new_a = rotate_box_cw b (a,n)
    if not $ aabb new_a b then wallBounce rest c else do
      e `set` ()
wallBounce
  ((b, n, Floor, _, eb):rest)
  c@((a,_),_,(v@(Velocity vel@(V2 vx vy)),p,_,_),e,_,Player)
  = do
    let chkPos i = rotate_box_cw b ((\(Velocity vel) (Box (o,x,y)) -> Box ((normalize vel / pure i) + o, x, y)) v a, n) -- the new player box that is obtained via rotating it by the wall's angle
    let em = edgeMeasures (chkPos 1) b 
    if not $ any ((`aabb` b ). chkPos) [1.0] then wallBounce rest c else do
      --eb `set` Sing
      when (norm vel < 1) $ e `set` Plant
      case minni (abs <$> em) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
        (d,TopEdge) -> e `set` reflect_vel TopEdge d n a ((Velocity $ V2 vx $ 0.5 * vy), p)
        (d,g) ->  e `set` reflect_vel g d n a (Velocity (pure 0.9) * v, p)
wallBounce
  ((b, n, OneWayWall, _, _):rest)
  c@((a,_),_,(v@(Velocity vel),p,_,_),e,_,Player)
  = do
  let chkPos i = rotate_box_cw b ((\(Velocity vel) (Box (p,x,y)) -> Box ((normalize vel / pure i) + p, x, y)) v a, n) -- the new player box that is obtained via rotating it by the wall's angle
  let ems i = edgeMeasures (chkPos i) b
  if (not $ any ((`aabb` b ) . chkPos) [1.0]) then wallBounce rest c else do
    when (norm vel < 1) $ e `set` Plant
    case (\i -> minni (abs <$> ems i) [RightEdge, LeftEdge, TopEdge, BottomEdge]) <$> [0.5,2.0] of
      [(d1,TopEdge),(d2,TopEdge)] -> 
        if d1 > d2 then set e $ reflect_vel TopEdge d1 n a (v, p) else  wallBounce rest c
      _ -> wallBounce rest c
wallBounce
  ((b, n, _, _, eb):rest)
  c@((a,_),_,(v@(Velocity vel),p,_,_),e,_,Player)
  = do
    let chkPos i = rotate_box_cw b ((\(Velocity vel) (Box (p,x,y)) -> Box ((normalize vel / pure i) + p, x, y)) v a, n) -- the new player box that is obtained via rotating it by the wall's angle
    let em = edgeMeasures (chkPos 1) b -- the distances to this box's edge
    if not $ any ((`aabb` b ). chkPos) [1.0] then wallBounce rest c else do
      --eb `set` Sing
      when (norm vel < 1) $ e `set` Plant
      --shove (b,eb) (a,e)
      case minni (abs <$> em) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
        (d,g) -> e `set` reflect_vel g d n a (v, p)
-- wallBounce
--   ((b, n, _, _, _):rest)
--   c@((a,_),_,(v,p,_,_),e,_,Enemy)
--   = do
--     let chkPos i = rotate_box_cw b ((\(Velocity vel) (Box (pos,x,y)) -> Box ((normalize vel / pure i) + pos, x, y)) v a, n) -- the new player box that is obtained via rotating it by the wall's angle
--     let em = edgeMeasures (chkPos 1) b
--     if not $ any ((`aabb` b ). chkPos) [0.5,2.0] then wallBounce rest c else do
--       {--get global >>= \(BoardControl _ bdlk _) -> when ( bdlk == Unlocked ) 
--         (eb `modify` 
--           (\sfx2@(SFXResources _ s2 _) -> 
--             if getInst s == getInst s2 
--             then sfx1
--             else sfx2
--           )
--         )--}
--       --eb `set` Sing
--       case minni (abs <$> em) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
--         (d,g) ->  e `set` reflect_vel g d n a (v, p)
wallBounce _ _ = return ()
