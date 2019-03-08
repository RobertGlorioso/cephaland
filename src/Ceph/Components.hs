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

import Ceph.Util
import Apecs
import Graphics.Gloss
import Language.Haskell.TH
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

data Actor = Player | Enemy | Wall | Weapon | Projectile deriving (Show,Eq)
instance Component Actor where
  type Storage Actor = Map Actor

data Wall = Wall1 deriving (Eq, Show)
instance Component Wall where
  type Storage Wall = Map Wall

data Weapon = Sword | Lance | Harpoon | Chain deriving (Eq, Show)
instance Component Weapon where
  type Storage Weapon = Map Weapon

data Enemy = Enemy1 deriving (Eq, Show)
instance Component Enemy where
  type Storage Enemy = Map Enemy

data Player = Player1 | Player2 | OtherPlayer deriving (Eq, Show)
instance Component Player where
  type Storage Player = Unique Player

data Linked = Linked Entity Entity deriving (Eq, Show, Ord)
instance Component Linked where
  type Storage Linked = Map Linked

data Projectile = Bullet | Arrow deriving (Eq,Show)
instance Component Projectile where
  type Storage Projectile = Map Projectile

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

data Behavior = Seek | Sing | Attack | Carry | Defend | Dead | Heal | Plant | NoBehavior deriving (Show,Eq)
instance Component Behavior where
  type Storage Behavior = Map Behavior

data UserInput = UI { uiEvent :: Event, time :: DiffTime }

data History = History [UserInput]
instance Component History where
  type Storage History = Map History

newtype Health = Health Float deriving (Eq, Num, Ord)
instance Component Health where
  type Storage Health = Map Health

data Sprite = Sprite [Picture]
instance Component Sprite where
  type Storage Sprite = Map Sprite
  
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
instance Component Velocity where type Storage Velocity =  Cache 100 (Map Velocity)

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
instance Monoid Beat where mempty = Beat 15 0

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

makeWorld "World" [''Camera, ''Scope, ''BodyPicture, ''Player, ''Enemy, ''Dummy, ''Wall, ''Projectile, ''Actor, ''Position, ''Linked, ''Velocity, ''Gravity, ''Angle, ''Target, ''Weapon, ''Charge, ''Dash, ''ProjCount, ''Song, ''Health, ''Box, ''Sprite, ''SFXResources, ''Beat, ''Debug, ''DebugMode, ''Behavior, ''Grid, ''ScreenBounds]

type Physics = (Position, Velocity, Angle, Box, Actor)

type Sound = (SFXResources, Song)

type Meta = (BodyPicture, Scope, Behavior)

keyActor = Key @"Actor"

keyGet k = gett k prod

liss =  (Proxy :: Proxy Actor) :#  (Proxy :: Proxy Angle) :# (Proxy :: Proxy Beat) :# (Proxy :: Proxy Scope) :# (Proxy :: Proxy Velocity) :# (Proxy :: Proxy Position) :# HNil

prod = insert (Key @"actor") (Proxy :: Proxy Actor) $ insert (Key @"angle") (Proxy :: Proxy Angle) $ insert (Key @"beat") (Proxy :: Proxy Beat) $ insert (Key @"scope") (Proxy :: Proxy Scope) $ insert (Key @"vel") (Proxy :: Proxy Velocity) $ insert (Key @"pos") (Proxy :: Proxy Position) nil
