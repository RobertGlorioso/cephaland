module Ceph.Component.Player where
  
import Ceph.Util
import Ceph.Physics.Box
import Ceph.Component.Projectile
import Ceph.Components
import Ceph.Component.Weapon
import Apecs
import Linear
import Control.Monad
import Foreign.C.Types

playerLoop :: (Player, Dash, Velocity, Box, Behavior, Charge, Entity) -> System World ()
playerLoop (Player1, _, _, _, Swinging, _, e) = do
  (chex,_) <- chainExtended 30
  (chex2,(p1,pn)) <- chainExtended 50
  if chex then
    e `modify`
      (\(Velocity v) ->
        (Velocity $ v + ( (if chex2 then 0.02 else 0.002) * normalize  (pn - p1))
        , Swinging))
    else return ()
playerLoop (Player1, _, _, (Box (p1@(V2 x1 _), _, _)), Attack, _, _) =
  cmapM_ $ \case
    Sword -> do
      Target tp@(V2 x2 _) <- get global
      cmap $ showSword x1 x2 tp p1
    _ -> return ()
playerLoop (Player1, _, _, b@(Box (p1,_,_)), Carry, _, _) =
  conceIf
    (\(p, a) -> aabb b p && a == Wall)
    (\(Box (_,x,y)) -> (Box (p1, x, y), Position p1))
  --allows the player to pick up an enemy or other entity
      --e <- return . filter (\(p, _, a) -> aabb b p && a == Wall) =<< (getAll :: System World [(Box, Entity, Actor)])
      --if length e == 0 then return () else do
        --let (_,carriedEnt,_) = head e
        --carriedEnt `modify` (\(Box (_,x,y)) -> Box (p1, x, y))
        --carriedEnt `set` (Position p1)
playerLoop (Player1, Dash dx, Velocity v, (Box (p1,_,_)), _, Charge cv chging, e) = do
  cmap $ \(Target o) -> ( Target (o + v), Position (o + v))
  cmapM_ $ \case
    (blt,Bullet) -> cmap (hurtPlayer blt)
    _ -> return ()
  Target tg <- get global
  let chg
        | chging = if cv < 10 then (Charge (cv + 0.01) True) else (Charge cv False)
        | True   = Charge cv False
  let dsh
        | dx < 10.0 = Dash (dx + 0.1)
        | True     = Dash dx
  e `set` (Player, Angle (v2ToRad $ p1 - tg), chg, dsh, Velocity v )     

player1 :: Txtr -> SFXResources -> System World Entity
player1 txtr s = 
  newEntity ((Position (V2 0 50)
              , 0 :: Velocity
              , Gravity (V2 0 (0.1))
              , txtr
              , Box (0, 1, 1))
            , (ProjCount 30, Health 99, Dash 0)
            , (Player1, Player)
            , (NoBehavior, Charge 0.01 True, s))

playerShootArrow :: (Charge, Position, Velocity, ProjCount, Player, Entity) -> System World ()
playerShootArrow (c, x, v, ProjCount arrowsLeft, Player1, e) = do
        t <- get global 
        when (arrowsLeft >= 1) $ do
          shootArrow t x v c
          e `set` (Charge 1.0 False, ProjCount $ arrowsLeft - 1)

playerShootChain :: (Charge, Position, Velocity, Player) -> System World (Charge, Behavior)
playerShootChain (c, x, v, Player1) = do
        t <- get global 
        shootChains t x v c
        return (Charge 1.0 False,Swinging)

playerSpeedLimit :: CDouble
playerSpeedLimit = 8

playerSwinging :: System World ()
playerSwinging = do
  --ls <- cfoldM (\a b -> return (b:a)) [] :: System World [(Linked, (Position, Entity))]
  tt <- cfoldM (\a b -> return (b:a)) []
  let [(Target t)] = tt 
  conceIfM_
    (\case
        (_, Out, _, _) -> False
        (_ :: Wall, In, n, c) -> let new_b = (rotate_box_cw c (Box (t, 0.5, 0.5),n)) in aabb new_b c
    )
    (\(Wall1) -> do
        cmap $ \(Player1) -> Swinging 
    )
    
playerDash :: Target -> (Player, Position, Velocity, Dash) -> (Player, Position, Velocity, Dash)
playerDash (Target o) pp@(Player1, Position p,_, Dash w) = do
  if w >= 2.0
  then (Player1, Position p, Velocity $ normalize (o - p), Dash (-2.0))
  else pp      

hurtPlayer  :: (Box,Projectile) -> (Box, Health, Player) -> Health
hurtPlayer ((Box painBox),Bullet) (Box plBox, h, _)  = if (aabb (Box painBox) (Box plBox)) then (h - 10) else h
hurtPlayer (_,_) (_, h, _)  = h
