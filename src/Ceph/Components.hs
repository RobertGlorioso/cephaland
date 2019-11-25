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
import Euterpea
import Linear
import Data.IntMap hiding (insert)
import qualified SDL.Mixer as M
import qualified SDL as S
import Foreign.C.Types
import GHC.Word

data GameOpts = GameOpts { debugOn :: Bool }

newtype Debug = Debug String deriving (Show)
instance Component Debug where
  type Storage Debug = Map Debug

data DebugMode = DebugMode Int deriving (Show,Eq)
instance Component DebugMode where
  type Storage DebugMode = Global DebugMode

instance Monoid DebugMode where
  mempty = DebugMode 0
instance Semigroup DebugMode where
  (<>) = const
  
--these are the guys in the game. if we want to use all these guys in the game loop we use this type.
--`cmap :: Actor -> ...` will map in everything where `cmap :: Wall -> ...` will map just walls
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
  
data Dummy = Dummy 
instance Component Dummy where
  type Storage Dummy = Unique Dummy

data Animated = Animate Int | Loop | Still
instance Component Animated where
  type Storage Animated = Map Animated

data Behavior = Seek | Sing | Attack | Carry | Defend | Dead | Heal | Plant | Swinging | NoBehavior | Moving (V2 CDouble) deriving (Show,Eq)
instance Component Behavior where
  type Storage Behavior = Map Behavior

newtype Health = Health Float deriving (Eq, Num, Ord)
instance Component Health where
  type Storage Health = Map Health

data SBoard a = SBoard
  (a, a, a, a)
  (a, a, a, a)
  (a, a, a, a)
  (a, a, a, a) deriving (Eq,Functor,Foldable,Traversable)

data BoardControl = BoardControl { status :: BoardStatus, lock :: BoardLock}
instance Component BoardControl where
  type Storage BoardControl = Unique BoardControl

data BoardStatus = Play | Pause deriving (Eq)
instance Component BoardStatus where
  type Storage BoardStatus = Unique BoardStatus

data BoardLock = Locked | Unlocked deriving (Eq)
instance Component BoardLock where
  type Storage BoardLock = Unique BoardLock

data SRow = S1 | S2 | S3 | S4
  deriving (Show, Eq, Enum, Ord)
           
data SColumn = SI | SII | SIII | SIV
  deriving (Show, Eq, Enum, Ord)

--so this is like a sequencer          
data SCoordF a = SCoordF SRow SColumn a
  deriving (Show, Eq, Functor)
type SCoord = SCoordF ()

instance Enum (SCoordF ()) where
  fromEnum (SCoordF S4 SI _) = 12
  fromEnum (SCoordF S4 SII _) = 13
  fromEnum (SCoordF S4 SIII _) = 14
  fromEnum (SCoordF S4 SIV _) = 15 
  fromEnum (SCoordF S3 SI _) = 8 
  fromEnum (SCoordF S3 SII _) = 9
  fromEnum (SCoordF S3 SIII _) = 10
  fromEnum (SCoordF S3 SIV _) = 11 
  fromEnum (SCoordF S2 SI _) = 4
  fromEnum (SCoordF S2 SII _) = 5
  fromEnum (SCoordF S2 SIII _) = 6
  fromEnum (SCoordF S2 SIV _) = 7
  fromEnum (SCoordF S1 SI _) = 0
  fromEnum (SCoordF S1 SII _) = 1
  fromEnum (SCoordF S1 SIII _) = 2
  fromEnum (SCoordF S1 SIV _) = 3
  toEnum 12 = SCoordF S4 SI ()
  toEnum 13 = SCoordF S4 SII ()
  toEnum 14 = SCoordF S4 SIII ()
  toEnum 15  = SCoordF S4 SIV ()
  toEnum 8  = SCoordF S3 SI ()
  toEnum 9 = SCoordF S3 SII ()
  toEnum 10 = SCoordF S3 SIII ()
  toEnum 11  = SCoordF S3 SIV ()
  toEnum 4 = SCoordF S2 SI ()
  toEnum 5 = SCoordF S2 SII ()
  toEnum 6 = SCoordF S2 SIII ()
  toEnum 7 = SCoordF S2 SIV ()
  toEnum 0 = SCoordF S1 SI ()
  toEnum 1 = SCoordF S1 SII ()
  toEnum 2 = SCoordF S1 SIII ()
  toEnum 3 = SCoordF S1 SIV ()
  
instance Monoid (SCoordF ()) where
  mempty = SCoordF S1 SI ()
instance Semigroup (SCoordF ()) where
  (<>) = const
    
instance Component (SCoordF ()) where
  type Storage (SCoordF ()) = Global (SCoordF ())

type Sequencer = SBoard Entity

instance Component (Sequencer) where
  type Storage (Sequencer) = Global (Sequencer)
instance Monoid (Sequencer) where
  mempty = SBoard (1,2,3,4) (5,6,7,8) (9,10,11,12) (13,14,15,16)
  mappend = const

instance Semigroup Sequencer where
  (<>) = const
  
data MBoard a = MBoard
  a a a a
  deriving (Eq,Functor)

data MCoordF a = MCoordF [Pitch] InstrumentName (a)
  deriving (Show, Eq, Functor)
type MCoord = MCoordF ()

data IBoard a = IBoard
  a a a a a a a a a a a a
  deriving (Show,Eq,Functor)

--this would be like an Instrument 
data ICoordF a = ICoordF Pitch Dur (a)
  deriving (Show, Eq, Functor)
type ICoord = ICoordF ()

data SpriteColor = SpriteColor (S.V4 Word8)
instance Component SpriteColor where
  type Storage SpriteColor = Map SpriteColor

data SDLRenderer = SDLRenderer S.Renderer
instance Component SDLRenderer where
  type Storage SDLRenderer = Unique SDLRenderer  

data SFXResources = SFXResources { sound :: M.Chunk, beat :: M.Chunk, song :: [Music Pitch] } deriving (Show,Eq)
instance Component SFXResources where
  type Storage SFXResources = Map SFXResources

data Box = Box (V2 CDouble, CDouble, CDouble) deriving (Show)
instance Component Box where
  type Storage Box = Map Box

data Phys = Phys { pos :: Position, vel :: Velocity, ang :: Angle, grav :: Gravity } deriving (Eq, Show)
instance Component Phys where type Storage Phys = Map Phys

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
  
newtype Song = Song (Music Pitch) deriving Show
instance Component Song where type Storage Song = Map Song

makeWorld "World" [''SDLRenderer, ''Sequencer, ''SCoord, ''Camera, ''Scope, ''Txtr, ''SpriteColor, ''BoardControl, ''BoardLock, ''BoardStatus, ''Player, ''Enemy, ''Dummy, ''Wall, ''Projectile, ''Actor, ''Position, ''Linked, ''Velocity, ''Gravity, ''Angle, ''Target, ''Weapon, ''Charge, ''Dash, ''ProjCount, ''Song, ''Health, ''Box, ''SFXResources, ''Beat, ''Debug, ''DebugMode, ''Behavior, ''Grid, ''ScreenBounds, ''Animated]

{--keyActor = Key @"Actor"

keyGet k = gett k prod

liss =  (Proxy :: Proxy Actor) :#  (Proxy :: Proxy Angle) :# (Proxy :: Proxy Beat) :# (Proxy :: Proxy Scope) :# (Proxy :: Proxy Velocity) :# (Proxy :: Proxy Position) :# HNil

prod = insert (Key @"actor") (Proxy :: Proxy Actor) $ insert (Key @"angle") (Proxy :: Proxy Angle) $ insert (Key @"beat") (Proxy :: Proxy Beat) $ insert (Key @"scope") (Proxy :: Proxy Scope) $ insert (Key @"vel") (Proxy :: Proxy Velocity) $ insert (Key @"pos") (Proxy :: Proxy Position) nil
--}
