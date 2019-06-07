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

import Apecs
import Graphics.Gloss
import Euterpea
import Linear
import Data.Semigroup
import Data.IntMap hiding (insert)
import Graphics.Gloss.Interface.IO.Game
import Data.Time.Clock
import qualified SDL.Mixer as M

data GameOpts = GameOpts { debugOn :: Bool }

newtype Debug = Debug String deriving (Show)
instance Component Debug where
  type Storage Debug = Map Debug

data DebugMode = DebugMode Int deriving (Show,Eq)
instance Component DebugMode where
  type Storage DebugMode = Global DebugMode

instance Monoid DebugMode where
  mempty = DebugMode 0

--these are the guys in the game. if we want to use all these guys in the game loop we use this type.
--`cmap :: Actor -> ...` will loop in everything where `cmap :: Wall -> ...` will loop in just walls
data Actor = Player | Enemy | Wall | Weapon | Projectile deriving (Show,Eq)
instance Component Actor where
  type Storage Actor = Map Actor

--a wall is a stationary box 
data Wall = Wall1 deriving (Eq, Show)
instance Component Wall where
  type Storage Wall = Map Wall

--the weapons: sword / lance will be a box that does cut / pierce | laser will reflect off boxes | harpoon will shoot tiny spears | chain will attach to boxes
data Weapon = Sword | Lance | Laser | Harpoon | Chain deriving (Eq, Show)
instance Component Weapon where
  type Storage Weapon = Map Weapon

--the enemies
data Enemy = Enemy1 deriving (Eq, Show)
instance Component Enemy where
  type Storage Enemy = Map Enemy

--the players
data Player = Player1 | Player2 | OtherPlayer deriving (Eq, Show)
instance Component Player where
  type Storage Player = Map Player

--the stuff getting shot 
data Projectile = Bullet | Arrow deriving (Eq,Show)
instance Component Projectile where
  type Storage Projectile = Map Projectile

--links between two entities and weighted links to add some variance
data Linked = Linked Entity Entity | WLinked Entity Entity Float deriving (Eq, Show, Ord)
instance Component Linked where
  type Storage Linked = Map Linked

data ProjCount = ProjCount Int deriving Show
instance Component ProjCount where
  type Storage ProjCount = Map ProjCount

data Charge = Charge { chgAmt :: Float, charging :: Bool } 
instance Component Charge where
  type Storage Charge = Map Charge

data Target = Target (V2 Float) deriving (Eq, Show)
instance Component Target where
  type Storage Target = Unique Target

data Dash = Dash Float deriving (Eq, Show)
instance Component Dash where
  type Storage Dash = Unique Dash
  
data Dummy = Dummy 
instance Component Dummy where
  type Storage Dummy = Unique Dummy

data Animated = Animate Int | Loop | Still
instance Component Animated where
  type Storage Animated = Map Animated

data Behavior = Seek | Sing | Attack | Carry | Defend | Dead | Heal | Plant | Swinging | NoBehavior | Moving (V2 Float) deriving (Show,Eq)
instance Component Behavior where
  type Storage Behavior = Map Behavior

data UserInput = UI { uiEvent :: Event, time :: DiffTime }

data History = History [UserInput]
instance Component History where
  type Storage History = Map History

newtype Health = Health Float deriving (Eq, Num, Ord)
instance Component Health where
  type Storage Health = Map Health

data SBoard a = SBoard
  (a, a, a, a)
  (a, a, a, a)
  (a, a, a, a)
  (a, a, a, a) deriving (Eq,Functor,Foldable,Traversable)

data SRow = S1 | S2 | S3 | S4
  deriving (Show, Eq, Enum, Ord)
data SColumn = SI | SII | SIII | SIV
  deriving (Show, Eq, Enum, Ord)

--so this is like a sequencer          
data SCoordF a = SCoordF SRow SColumn (a)
  deriving (Show, Eq, Functor)
type SCoord = SCoordF ()

instance Enum (SCoord) where
  succ (SCoordF S4 SIV _) = SCoordF S1 SI ()
  succ (SCoordF i SIV _) = SCoordF (succ i) SI ()
  succ (SCoordF i j _) = SCoordF i (succ j) ()

instance Monoid (SCoordF ()) where
  mempty = SCoordF S1 SI ()
  
instance Component (SCoordF ()) where
  type Storage (SCoordF ()) = Global (SCoordF ())

type Sequencer = SBoard Entity --(Music Pitch, Picture)
instance Component (Sequencer) where
  type Storage (Sequencer) = Global (Sequencer)
instance Monoid (Sequencer) where
  mempty = SBoard (1,2,3,4) (5,6,7,8) (9,10,11,12) (13,14,15,16)

--different Functor types can make different interfaces
--this would be like an synthesizer 
data MBoard a = MBoard
  a a a a
  deriving (Eq,Functor)

data MCoordF a = MCoordF [Pitch] InstrumentName (a)
  deriving (Show, Eq, Functor)
type MCoord = MCoordF ()

data IBoard a = IBoard
  a a a a a a a a a a a a
  deriving (Eq,Functor)

--this would be like an Instrument 
data ICoordF a = ICoordF Pitch Dur (a)
  deriving (Show, Eq, Functor)
type ICoord = ICoordF ()

data Sprites = Sprites [Picture]
instance Component Sprites where
  type Storage Sprites = Map Sprites
  
data SFXResources = SFXResources { percussion :: [M.Chunk] , melody :: [M.Chunk] } deriving (Show,Eq)
instance Component SFXResources where
  type Storage SFXResources = Map SFXResources

data Box = Box (V2 Float, Float, Float) deriving (Show)
instance Component Box where
  type Storage Box = Map Box

newtype Grid = Grid (IntMap (IntMap ())) deriving Show
instance Component Grid where type Storage Grid = Unique Grid

newtype Position = Position (V2 Float) deriving (Num, Show)
instance Component Position where type Storage Position = Cache 100 (Map Position)

newtype Velocity = Velocity (V2 Float) deriving (Num, Show)
instance Component Velocity where type Storage Velocity = Cache 100 (Map Velocity)

newtype Gravity = Gravity (V2 Float) deriving Show
instance Semigroup Gravity where
  ((<>)) (Gravity a) (Gravity b) = Gravity $ a + b 
instance Monoid Gravity where
  mempty = Gravity 0
instance Component Gravity where type Storage Gravity = Global Gravity

newtype Angle = Angle {unAngle :: Float} deriving (Show,Eq,Num)
instance Component Angle where type Storage Angle = Map Angle

newtype BodyPicture = BodyPicture Picture deriving (Show, Eq)
instance Component BodyPicture where
  type Storage BodyPicture = Map BodyPicture

data Beat = Beat Int Int
instance Component Beat where type Storage Beat = Global Beat
instance Monoid Beat where mempty = Beat 16 0

data Scope = In | Out deriving (Eq,Show)
instance Component Scope where type Storage Scope = Map Scope

data Camera = Camera
  { cameraOffset :: V2 Float
  , cameraScale  :: Float
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
instance Component ScreenBounds where
  type Storage ScreenBounds = Global ScreenBounds
  
newtype Song = Song (Music Pitch) deriving Show
instance Component Song where type Storage Song = Map Song

makeWorld "World" [''Sequencer, ''SCoord, ''Camera, ''Scope, ''BodyPicture, ''Player, ''Enemy, ''Dummy, ''Wall, ''Projectile, ''Actor, ''Position, ''Linked, ''Velocity, ''Gravity, ''Angle, ''Target, ''Weapon, ''Charge, ''Dash, ''ProjCount, ''Song, ''Health, ''Box, ''Sprites, ''SFXResources, ''Beat, ''Debug, ''DebugMode, ''Behavior, ''Grid, ''ScreenBounds, ''Animated]

type Physics = (Position, Velocity, Angle, Box, Actor)

type Sound = (SFXResources, Song)

type Meta = (BodyPicture, Scope, Behavior)

{--keyActor = Key @"Actor"

keyGet k = gett k prod

liss =  (Proxy :: Proxy Actor) :#  (Proxy :: Proxy Angle) :# (Proxy :: Proxy Beat) :# (Proxy :: Proxy Scope) :# (Proxy :: Proxy Velocity) :# (Proxy :: Proxy Position) :# HNil

prod = insert (Key @"actor") (Proxy :: Proxy Actor) $ insert (Key @"angle") (Proxy :: Proxy Angle) $ insert (Key @"beat") (Proxy :: Proxy Beat) $ insert (Key @"scope") (Proxy :: Proxy Scope) $ insert (Key @"vel") (Proxy :: Proxy Velocity) $ insert (Key @"pos") (Proxy :: Proxy Position) nil
--}
