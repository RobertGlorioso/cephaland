{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ceph.Components where


import Data.Functor.Rep
import Data.Functor.Adjunction
import Data.Distributive
import Apecs
import Euterpea (Music(..),Pitch,Dur)
import Linear
import Data.IntMap hiding (insert,foldl)
import qualified SDL.Mixer as M
import qualified SDL as S
import Foreign.C.Types
import GHC.Word

data GameOpts = GameOpts { debugOn :: Bool }

data SDLRenderer = SDLRenderer S.Renderer
instance Component SDLRenderer where
  type Storage SDLRenderer = Unique SDLRenderer  

--these are the guys in the game. if we want to use all these guys in the game loop we use this type.
--`cmap :: Actor -> ...` will map in everything where `cmap :: Wall -> ...` will map just walls
data Actor = Player | Enemy | Wall | Weapon | Projectile deriving (Show,Eq)
instance Component Actor where
  type Storage Actor = Map Actor

data Orbiting = Orbiting Entity
instance Component Orbiting where
  type Storage Orbiting = Map Orbiting

--a wall is a stationary box 
data Wall = Wall1 | Wall2 | Wall3 | OneWayWall | Floor deriving (Eq, Show)
instance Component Wall where
  type Storage Wall = Map Wall

--the weapons: sword / lance will be a box that does cut / pierce | laser will reflect off boxes | harpoon will shoot tiny spears | chain will attach to boxes
data Weapon = Sword | Lance | Laser | Harpoon | Chain | Net deriving (Eq, Show)
instance Component Weapon where
  type Storage Weapon = Map Weapon

--the enemies
data Enemy = Enemy1 deriving (Eq, Show)
instance Component Enemy where
  type Storage Enemy = Map Enemy

--the players
data Player = Player1 deriving (Eq, Show)
instance Component Player where
  type Storage Player = Map Player

--the stuff getting shot 
data Projectile = Bullet | Arrow | Squall deriving (Eq,Show)
instance Component Projectile where
  type Storage Projectile = Map Projectile

--links between two entities and weighted links to add some variance
data Linked = Linked Entity Entity | WLinked Entity Entity CDouble deriving (Eq, Show, Ord)
instance Component Linked where
  type Storage Linked = Map Linked

data ProjCount = ProjCount Int deriving Show
instance Component ProjCount where
  type Storage ProjCount = Map ProjCount

data Charge = Charge { chgAmt :: CDouble, charging :: Bool } 
instance Component Charge where
  type Storage Charge = Map Charge

data Target = Target (V2 CDouble) deriving (Eq, Show)
instance Component Target where
  type Storage Target = Unique Target

data Dash = Dash Float deriving (Eq, Show)
instance Component Dash where
  type Storage Dash = Unique Dash
  
data Animated = Animate Int | Loop | Still
instance Component Animated where
  type Storage Animated = Map Animated

data Behavior = Seek | Sing | Attack | Carry | Defend | Trapped | Heal | Plant | Swinging | NoBehavior deriving (Show,Eq)
instance Component Behavior where
  type Storage Behavior = Map Behavior

newtype Health = Health Float deriving (Eq, Num, Ord)
instance Component Health where
  type Storage Health = Map Health

data SList a = SList [a] deriving (Eq, Functor, Foldable, Traversable)

instance Monoid (SList a) where
  mempty = SList []
instance Semigroup (SList a) where
  (SList a) <> (SList b) = SList $ a ++ b

type SongList = SList SFXResources
instance Component SongList where
  type Storage SongList = Unique SongList

data SBoard a = SBoard
  (a, a, a, a)
  (a, a, a, a)
  (a, a, a, a)
  (a, a, a, a) deriving (Eq,Functor,Foldable,Traversable)

data BoardControl = BoardControl {status :: BoardStatus, lock :: BoardLock}
instance Component BoardControl where
  type Storage BoardControl = Unique BoardControl

data BoardStatus = Play | Pause deriving (Eq)
instance Component BoardStatus where
  type Storage BoardStatus = Unique BoardStatus

data BoardLock = Locked | Unlocked deriving (Eq)
instance Component BoardLock where
  type Storage BoardLock = Unique BoardLock

data SpriteColor = SpriteColor (S.V4 Word8) deriving (Eq, Show)
instance Component SpriteColor where
  type Storage SpriteColor = Map SpriteColor

data SFXResources = SFXResources { sound :: M.Chunk, song :: Music Pitch, clr :: SpriteColor } deriving (Show,Eq)
instance Component SFXResources where
  type Storage SFXResources = Map SFXResources

data SRow = S1 | S2 | S3 | S4
  deriving (Show, Eq, Enum, Ord)
           
data SColumn = SI | SII | SIII | SIV
  deriving (Show, Eq, Enum, Ord)

--so this is like a sequencer          
data SCoordF a = SCoordF SRow SColumn a
  deriving (Show, Eq, Functor)

instance Enum (SCoordF ()) where
  fromEnum (SCoordF row col _) = 4*(fromEnum row) + fromEnum col
  toEnum n 
    | n < 16 && n > 0 = SCoordF (toEnum $ n `div` 4) (toEnum $ n `mod` 4) ()
    | otherwise        = SCoordF S1 SI ()

instance Monoid (SCoordF ()) where
  mempty = SCoordF S1 SI ()
instance Semigroup (SCoordF ()) where
  (<>) = const

type SCoord = SCoordF ()
instance Component (SCoord) where
  type Storage (SCoord) = Global (SCoord)

type Sequencer = SBoard Entity

instance Component (Sequencer) where
  type Storage (Sequencer) = Map (Sequencer)
instance Monoid (Sequencer) where
  mempty = SBoard (1,2,3,4) (5,6,7,8) (9,10,11,12) (13,14,15,16)
  mappend = const

instance Semigroup Sequencer where
  (<>) = const
  
data MBoard a = MBoard [(V2 CDouble, a)] Bool
  deriving (Functor, Foldable, Traversable)

type Netitor = MBoard SFXResources

instance Component Netitor where
  type Storage Netitor = Unique Netitor

data MCoordF a = MCoordF (S.Point V2 CDouble) a
  deriving (Show, Eq, Functor)

type MCoord = MCoordF ()
instance Component MCoord where
  type Storage MCoord = Unique MCoord


instance Distributive MBoard where
  distribute = distributeRep

instance Representable MBoard where
  -- We index into our functor using Coord
  type Rep MBoard = MCoord
  -- Given an index and a board, pull out the matching cell
  index (MBoard objs _) (MCoordF (S.P v) _) = snd $ foldl 
    (\(minb,gx) (b,fx) -> let d = S.distance v b; mind = S.distance v minb in if mind < d then (minb,gx) else (b,fx)) 
    (head objs) 
    (tail objs)

  tabulate desc = MBoard mempty False

instance Adjunction MCoordF MBoard where
  unit a = tabulate (\(MCoordF v _ ) -> MCoordF v a)
  counit (MCoordF v board) = index board (MCoordF v ())

{--
defineIBoard :: ICoord -> Music Pitch
defineIBoard (ICoordF p m _) = Prim . Note m $ p

instance Distributive IBoard where
  distribute = distributeRep

instance Representable IBoard where
  type Rep IBoard = ICoord
  index (IBoard a _ _ _ _ _ _ _ _ _ _ _) (ICoordF (A,_) _ _) = a
  index (IBoard _ a _ _ _ _ _ _ _ _ _ _) (ICoordF (As,_) _ _) = a
  index (IBoard _ _ a _ _ _ _ _ _ _ _ _) (ICoordF (B,_) _ _) = a
  index (IBoard _ _ _ a _ _ _ _ _ _ _ _) (ICoordF (Bs,_) _ _) = a
  index (IBoard _ _ _ _ a _ _ _ _ _ _ _) (ICoordF (C,_) _ _) = a
  index (IBoard _ _ _ _ _ a _ _ _ _ _ _) (ICoordF (Cs,_) _ _) = a
  index (IBoard _ _ _ _ _ _ a _ _ _ _ _) (ICoordF (D,_) _ _) = a
  index (IBoard _ _ _ _ _ _ _ a _ _ _ _) (ICoordF (Ds,_) _ _) = a
  index (IBoard _ _ _ _ _ _ _ _ a _ _ _) (ICoordF (E,_) _ _) = a
  index (IBoard _ _ _ _ _ _ _ _ _ a _ _) (ICoordF (Es,_) _ _) = a
  index (IBoard _ _ _ _ _ _ _ _ _ _ a _) (ICoordF (F,_) _ _) = a
  index (IBoard _ _ _ _ _ _ _ _ _ _ _ a) (ICoordF (G,_) _ _) = a
  tabulate desc = IBoard (desc (ICoordF (A,4) qn ())) (desc (ICoordF (As,4) qn ())) 
    (desc (ICoordF (B,4) qn ())) 
    (desc (ICoordF (Bs,4) qn ())) 
    (desc (ICoordF (C,4) qn ())) 
    (desc (ICoordF (Cs,4) qn ())) 
    (desc (ICoordF (D,4) qn ())) 
    (desc (ICoordF (Ds,4) qn ())) 
    (desc (ICoordF (E,4) qn ())) 
    (desc (ICoordF (Es,4) qn ())) 
    (desc (ICoordF (F,4) qn ())) 
    (desc (ICoordF (G,4) qn ()))
  
instance Adjunction ICoordF IBoard where
  unit a = tabulate (\(ICoordF row col _ ) -> ICoordF row col a)
  counit (ICoordF row col board) = index board (ICoordF row col ()) --}

instance (Show a) => Show (SBoard a) where
  show (SBoard a b c d) = "       I  |  II | III | IV\n"
    ++ "A   " ++ show a ++ "\n"
    ++ "B   " ++ show b ++ "\n"
    ++ "C   " ++ show c ++ "\n"
    ++ "D   " ++ show d ++ "\n"
    
instance Distributive SBoard where
  distribute = distributeRep
  
instance Adjunction SCoordF SBoard where
  --unit :: a -> SBoard (SCoordF a)
  unit a = tabulate (\(SCoordF row col _ ) -> SCoordF row col a)
  counit (SCoordF row col board) = index board (SCoordF row col ())

instance Representable SBoard where
  -- We index into our functor using Coord
  type Rep SBoard = SCoord
  -- Given an index and a board, pull out the matching cell
  index (SBoard (a, _, _, _) _ _ _) (SCoordF S1 SI _) = a
  index (SBoard (_, a, _, _) _ _ _) (SCoordF S1 SII _) = a
  index (SBoard (_, _, a, _) _ _ _) (SCoordF S1 SIII _) = a
  index (SBoard (_, _, _, a) _ _ _) (SCoordF S1 SIV _) = a
  index (SBoard _ (a, _, _, _) _ _) (SCoordF S2 SI _) = a
  index (SBoard _ (_, a, _, _) _ _) (SCoordF S2 SII _) = a
  index (SBoard _ (_, _, a, _) _ _) (SCoordF S2 SIII _) = a
  index (SBoard _ (_, _, _, a) _ _) (SCoordF S2 SIV _) = a
  index (SBoard _ _ (a, _, _, _) _) (SCoordF S3 SI _) = a
  index (SBoard _ _ (_, a, _, _) _) (SCoordF S3 SII _) = a
  index (SBoard _ _ (_, _, a, _) _) (SCoordF S3 SIII _) = a
  index (SBoard _ _ (_, _, _, a) _) (SCoordF S3 SIV _) = a
  index (SBoard _ _ _ (a, _, _, _)) (SCoordF S4 SI _) = a
  index (SBoard _ _ _ (_, a, _, _)) (SCoordF S4 SII _) = a
  index (SBoard _ _ _ (_, _, a, _)) (SCoordF S4 SIII _) = a
  index (SBoard _ _ _ (_, _, _, a)) (SCoordF S4 SIV _) = a

  -- Given a function which describes a slot, build a Board
  tabulate desc = SBoard
      (desc (SCoordF S1 SI ()), desc (SCoordF S1 SII ()), desc (SCoordF S1 SIII ()), desc (SCoordF S1 SIV ()))
      (desc (SCoordF S2 SI ()), desc (SCoordF S2 SII ()), desc (SCoordF S2 SIII ()), desc (SCoordF S2 SIV ()))
      (desc (SCoordF S3 SI ()), desc (SCoordF S3 SII ()), desc (SCoordF S3 SIII ()), desc (SCoordF S3 SIV ()))
      (desc (SCoordF S4 SI ()), desc (SCoordF S4 SII ()), desc (SCoordF S4 SIII ()), desc (SCoordF S4 SIV ()))

data IBoard a = IBoard
  a a a a a a a a a a a a
  deriving (Show,Eq,Functor)

--this would be like an Instrument 
data ICoordF a = ICoordF Pitch Dur (a)
  deriving (Show, Eq, Functor)
type ICoord = ICoordF ()

data Box = Box (V2 CDouble, CDouble, CDouble) deriving (Show)
instance Component Box where
  type Storage Box = Map Box

type Phys = (Velocity, Position, Gravity, Angle)

newtype AngularMomentum = AngularMomentum (CDouble)
instance Component AngularMomentum where type Storage AngularMomentum = Map AngularMomentum

newtype Grid = Grid (IntMap (IntMap ())) deriving Show
instance Component Grid where type Storage Grid = Unique Grid

newtype Position = Position (V2 CDouble) deriving (Num, Eq, Show)
instance Component Position where type Storage Position = Cache 100 (Map Position)
instance Semigroup Position where
  ((<>)) (Position a) (Position b) = Position $ a + b 
instance Monoid Position where
  mempty = Position 0

newtype Velocity = Velocity (V2 CDouble) deriving (Num, Eq, Show)
instance Component Velocity where type Storage Velocity = Cache 100 (Map Velocity)

newtype Gravity = Gravity (V2 CDouble) deriving (Eq, Show)
instance Semigroup Gravity where
  ((<>)) (Gravity a) (Gravity b) = Gravity $ a + b 
instance Monoid Gravity where
  mempty = Gravity 0
instance Component Gravity where type Storage Gravity = Map Gravity

newtype Angle = Angle {unAngle :: CDouble} deriving (Show,Eq,Num)
instance Component Angle where type Storage Angle = Map Angle

data Txtr = Txtr S.Texture (S.Rectangle CInt) deriving Eq
instance Component Txtr where type Storage Txtr = Map Txtr

data Beat = Beat Int Int deriving Show
instance Component Beat where 
  type Storage Beat = Global Beat
instance Monoid Beat where 
  mempty = Beat 16 0
  mappend = const

instance Semigroup Beat where
  (<>) = const
  
data Scope = In | Out deriving (Eq,Show)
instance Component Scope where type Storage Scope = Map Scope

data Camera = Camera
  { cameraOffset :: V2 CDouble
  , cameraScale  :: CDouble
  }

instance Monoid Camera where
  mempty = Camera 0 1
  mappend (Camera a _) (Camera b c) = Camera ((a + b) / 2) c
  
instance Semigroup Camera where
  ((<>)) (Camera a _) (Camera b c) = Camera ((a + b) / 2) c
  
instance Component Camera where
  type Storage Camera = Global Camera

newtype ScreenBounds = SB (V2 Int) deriving (Eq, Show)
instance Monoid ScreenBounds where
  mempty = SB (V2 600 480)
  mappend = const

instance Semigroup ScreenBounds where
  (<>) = const
  
instance Component ScreenBounds where
  type Storage ScreenBounds = Global ScreenBounds
  

makeWorld "World" [''SDLRenderer, ''AngularMomentum, ''SongList, ''Sequencer, ''SCoord, ''MCoord, ''Camera, ''Scope, ''Txtr, ''BoardControl, ''BoardLock, ''BoardStatus, ''Player, ''Enemy, ''Wall, ''Projectile, ''Actor, ''Position, ''Linked, ''Velocity, ''Gravity, ''Angle, ''Target, ''Weapon, ''Charge, ''Dash, ''ProjCount, ''Health, ''Box, ''SFXResources, ''Netitor, ''Beat, ''Behavior, ''Grid, ''ScreenBounds, ''Animated]

{--keyActor = Key @"Actor"

keyGet k = gett k prod

liss =  (Proxy :: Proxy Actor) :#  (Proxy :: Proxy Angle) :# (Proxy :: Proxy Beat) :# (Proxy :: Proxy Scope) :# (Proxy :: Proxy Velocity) :# (Proxy :: Proxy Position) :# HNil

prod = insert (Key @"actor") (Proxy :: Proxy Actor) $ insert (Key @"angle") (Proxy :: Proxy Angle) $ insert (Key @"beat") (Proxy :: Proxy Beat) $ insert (Key @"scope") (Proxy :: Proxy Scope) $ insert (Key @"vel") (Proxy :: Proxy Velocity) $ insert (Key @"pos") (Proxy :: Proxy Position) nil
--}
