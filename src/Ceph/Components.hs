{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ceph.Components where


import Apecs
import Apecs.Util
import Graphics.Gloss
--import Numeric.Hamilton
import Euterpea
import Linear
import Data.Semigroup
import Data.IntMap
import Graphics.Gloss.Interface.IO.Game hiding ( Play )
import Data.Time.Clock
import qualified SDL.Mixer as M

data GameOpts = GameOpts { debugOn :: Bool }

newtype Weapon = Weapon InstrumentName deriving Show
instance Component Weapon where
  type Storage Weapon = Map Weapon

newtype Debug = Debug String deriving (Show)
instance Component Debug where
  type Storage Debug = Map Debug

{--
data Phases = PHS (Phase 1) | PHS2 (Phase 2)
instance Component Phases where
  type Storage Phases = Map Phases

data Ghost = Pend  (V2 Float) | OneBody  (V2 Float) | TwoBody  (V2 Float) deriving Eq
instance Component Ghost where
  type Storage Ghost = Map Ghost
--}

data Actor = Player1 | Enemy1 | Wall | Sword | Harpoon | Projectile deriving (Show,Eq)
instance Component Actor where
  type Storage Actor = Map Actor

data Enemy1 = Enemy
instance Component Enemy1 where
  type Storage Enemy1 = Map Enemy1

data Player1 = Player
instance Component Player1 where
  type Storage Player1 = Unique Player1

data Attacking = Attacking
instance Component Attacking where
  type Storage Attacking = Unique Attacking

data Charge = Charge { amt :: Float, charging :: Bool } 
instance Component Charge where
  type Storage Charge = Map Charge

data Target = Target (V2 Float)
instance Component Target where
  type Storage Target = Unique Target

data Dash = Dash Float
instance Component Dash where
  type Storage Dash = Unique Dash

data Projectile = Bullet | Arrow deriving Eq
instance Component Projectile where
  type Storage Projectile = Map Projectile

data ProjCount = ProjCount Int deriving Show
instance Component ProjCount where
  type Storage ProjCount = Map ProjCount

data Behavior = Seek | Sing | Attack | Carry | Defend | Dead | Heal | Plant | NoBehavior deriving (Show,Eq)
instance Component Behavior where
  type Storage Behavior = Map Behavior

data UserInput = UI { uiEvent :: Event, time :: DiffTime }

data History = History [UserInput]
instance Component History where
  type Storage History = Map History

data Health = Health Float
instance Component Health where
  type Storage Health = Map Health

data Resources = Resources { sprites :: [Picture] , soundEffects :: [M.Chunk] }
instance Component Resources where
  type Storage Resources = Map Resources

data Box = Box (V2 Float, Float, Float) deriving (Show)
instance Component Box where
  type Storage Box = Map Box

newtype Grid = Grid (IntMap (IntMap ())) deriving Show
instance Component Grid where type Storage Grid = Unique Grid

newtype Position = Position (V2 Float) deriving (Num, Show)
instance Component Position where type Storage Position = (Map Position) -- Cache 100 (Map Position)

newtype Velocity = Velocity (V2 Float) deriving (Num, Show)
instance Component Velocity where type Storage Velocity = Map Velocity

newtype Gravity = Gravity (V2 Float) deriving Show
instance Semigroup Gravity where
  ((<>)) (Gravity a) (Gravity b) = Gravity $ a + b 
instance Monoid Gravity where
  mempty = Gravity 0
instance Component Gravity where type Storage Gravity = Global Gravity

newtype Angle = Angle Float deriving Show
instance Component Angle where type Storage Angle = Map Angle

newtype BodyPicture = BodyPicture Picture

instance Component BodyPicture where
  type Storage BodyPicture = Map BodyPicture

data Beat = Beat Int Int
instance Component Beat where type Storage Beat = Global Beat
instance Monoid Beat where mempty = Beat 15 0

data Camera = Camera
  { gvOffset :: V2 Float
  , gvScale  :: Float
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

makeWorld "World" [''Camera, ''BodyPicture, ''Player1, ''Enemy1, ''Projectile, ''Actor, ''Position, ''Velocity, ''Gravity, ''Angle, ''Target, ''Attacking, ''Charge, ''Dash, ''ProjCount, ''Song, ''Weapon, ''Health, ''Box, ''Resources, ''Beat, ''Debug, ''Behavior, ''Grid, ''ScreenBounds]
