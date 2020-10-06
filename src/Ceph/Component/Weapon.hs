{-# LANGUAGE FlexibleContexts #-}
module Ceph.Component.Weapon where

import Ceph.Components
import Ceph.Physics.Box
import Ceph.Util
import Ceph.Scene

import Apecs
import Linear
import Data.List
import Control.Monad
import Data.Ord
import Foreign.C.Types
import qualified SDL as S

netLoop :: System World ()
netLoop = do
  cmapM $ \case
    (Net, nb@(Box (_,_,_)), pos@(Position np), d) -> do
      cmapM  $ \case
        (Enemy, eb, s :: SFXResources, e) -> 
          if (aabb eb nb) then do
            bounce (0.9,0.1) e d
            
            --global `modify` (\(SList ss) -> SList $ s:ss)
          else return ()
        (_,_,_,_) -> return ()
      Target t <- cfoldM (\_ b -> return b) (Target 0)
      let V2 w h = abs $ (/50) $ t - np
      return $ Box (np, w, h)
    (_,b,_,_) -> return b

showSword ::
  Ord a =>
  a
  -> a
  -> V2 CDouble
  -> V2 CDouble
  -> (Weapon, Position, Angle, Velocity)
  -> (Weapon, Position, Angle, Velocity)
showSword x1 x2 tp pl (Sword,_,_,_) =
  (Sword
  , Position $ if x2 > x1
               then pl + (V2 0.86 0.12)
               else pl + (V2 (-0.86) 0.12)
  , Angle $ v2ToRad ( tp - pl )
  , Velocity 0
  )
showSword _ _ _ _ a = a

hideSword :: (Weapon, Position) -> (Weapon, Position)
hideSword (Sword,_) = (Sword, Position $ pure 2e7 )
hideSword a = a

sword :: Txtr -> System World Entity
sword pic = newEntity 
            (pic
            , Position (V2 (-1.05) 9.66)
            , Velocity 0
            , Angle 0
            , Box ((V2 (-1.05) 9.66), 0.4, 0.21)
            , (Weapon,Sword))

laser :: Txtr -> System World Entity
laser pic = newEntity 
            (pic
            , Position (pure 2e7)
            , Velocity 0
            , Angle 0
            , Box ((pure 2e7), 0.04, 0.04)
            , (Weapon,Laser) )
           
harpoon :: Txtr -> System World Entity
harpoon pic = newEntity 
              (pic
              , Position (V2 (-10.05) 9.66)
              , Velocity 0
              , Angle 0
              , Box ((V2 (-1.05) 9.66), 0.04, 0.21)
              , (Weapon,Harpoon))


net :: Txtr -> Netitor -> System World Entity
net pic ntr = do
  newEntity ((Weapon, Net, ntr)
            , NoBehavior
            , Angle 0
            , Position 0
            , Velocity 0
            , (box 0 50 50
              --, pic
              )
            )

chain :: Txtr -> System World Entity
chain pic = do
  newEntity (Chain
            , Weapon
            , NoBehavior
            , Angle 0
            , Position 0
            , Velocity 0
            , (box 0 0.05 0.05
              , pic
              )
            )

chain' :: SFXResources -> System World Entity
chain' s = do
  newEntity (Chain
            , Weapon
            , NoBehavior
            , Angle 0
            , Position 0
            , Velocity 0
            , (box 0 0.05 0.05
              , s
              )
            )

chains :: [Entity] -> Entity -> System World ()
chains [] _ = return ()
chains [_] _ = return ()
chains (_:lst:[]) targ = lst `set` (WLinked lst targ 1.0 )
chains (prev:cur:nxt:rst) t = do
  cur `set` (Linked prev nxt)
  chains (cur:nxt:rst) t

wChains :: CDouble -> [Entity] -> Entity -> System World ()
wChains _ [] _ = return ()
wChains _ (_:lst:[]) targ = lst `set` (WLinked lst targ 1.0 )
wChains j (prev:cur:nxt:rst) t = do
  cur `set` (WLinked prev nxt j)
  chains (cur:nxt:rst) t
    
chainExtended ::
  CDouble -> System World (Bool, (V2 CDouble, V2 CDouble))
chainExtended r = do
  ls <- cfoldM (\a b -> return (b:a)) [] :: System World [(Linked, Position)]
  let (_, Position p1) = minimumBy (comparing fst) ls
  let (_, Position pn) = maximumBy (comparing fst) ls
  if norm ( p1 - pn ) > r then return (True,(p1,pn)) else return (False, (p1,pn)) 

moveNets :: (Netted,Box) -> System World (Position,Box)
moveNets (Netted ns, Box (_,w,h)) = do
  ps <- fmap (\(Position p) -> p) <$> mapM get ns
  let newP = (sum ps) / (fromIntegral $ length ps )
  return $ (Position newP, Box (newP, w, h))

moveChains :: (Linked, Position, Box) -> System World (Angle, Position, Box)
moveChains (Linked e f, _, Box (_,w,h)) = do
  (Position p1) <- get e
  (Position p0) <- get f
  let newP = (p0 + p1) / 2
  return $ (Angle $ v2ToRad (p0 - p1), Position newP, Box (newP,w,h)) 
moveChains (WLinked e f m, _, Box (_,w,h)) = do
  (Position p0) <- get e
  (Position p1) <- get f
  let newP = pure (1-m) * p0 + (pure m * p1)
  return $ (Angle $ v2ToRad (p0 - p1), Position newP, Box (newP,w,h)) 
moveChains (End e, Position p0, Box (_,w,h)) = do
  (Position p1) <- get e
  return $ (Angle $ v2ToRad (p0 - p1), Position p1, Box (p1,w,h))