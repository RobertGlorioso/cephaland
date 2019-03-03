{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
module Ceph.Util where

import Ceph.Components

import Control.Applicative
import Control.Monad
import Control.Monad.Reader hiding (forM_)
import Data.Proxy
import Fcf
import GHC.TypeLits hiding (type (*))
import Unsafe.Coerce
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.TH
--import Language.Haskell.TH
import Linear
import Apecs
import Apecs.Core
import qualified Data.Vector.Unboxed as U
--import GHC.Prim

v2ToRad :: (Floating a, Ord a) => V2 a -> a
v2ToRad (V2 m n) = case compare m 0 of
  LT -> atan ( n / m ) + pi
  GT -> atan ( n / m ) 
  EQ -> (-pi)/2 * (signum n)

kpow :: (Monad m, Num t, Eq t) => t -> (b -> m b) -> b -> m b
kpow 0 _ = return
kpow n f = f <=< kpow (n-1) f

fpow :: t2 -> Int -> (t2 -> t) -> (t2 -> t2) -> [t]
fpow a 0 _ _ = []
fpow a n f g = f a : fpow (g a) (n-1) f g

{-# INLINE cmapIf_ #-}
cmapIf_ :: forall w m cp cx cy.
  ( Get w m cx
  , Get w m cp
  , Members w m cx
  , Set w m cy )
  => (cp -> Bool)
  -> (cx -> cy)
  -> SystemT w m ()
cmapIf_ cond f = do
  sp :: Storage cp <- getStore
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  sl <- lift $ explMembers (sx,sp)
  slf <- filter (\(_,p) -> cond p ) <$>  forM (U.toList sl)
    (\e -> do
        p <- lift $ explGet sp e
        return (e,p)
    )
  
  lift $ forM_ slf $ \(e,_) -> do 
      x <- explGet sx e
      explSet sy e (f x)

{-# INLINE cmapIfM_ #-}
cmapIfM_ :: forall w m cp cx .
  ( Get w m cx
  , Get w m cp
  , Members w m cx
  , MonadIO m
  )
  => (cp -> Bool)
  -> (cx -> SystemT w m ())
  -> SystemT w m ()
cmapIfM_ cond f = do
  sp :: Storage cp <- getStore
  sx :: Storage cx <- getStore
  sl <- lift $ explMembers (sx,sp)
  slf <- filter (\(_,p) -> cond p ) <$>  forM (U.toList sl)
    (\e -> do
        p <- lift $ explGet sp e
        return (e,p)
    )
  liftIO . print $ length slf
  forM_ slf $ \(e,_) -> do 
      x <- lift $ explGet sx e
      f x


conceM_ :: forall w m cx. (Alternative m,Get w m cx, Members w m cx) => (cx -> SystemT w m ()) -> SystemT w m ()
conceM_ f =  do
  sx :: Storage cx <- getStore
  sl <- lift $ explMembers sx
  lift $ guard (U.null sl)
  x <- lift . explGet sx . U.head $ sl 
  f x

{-# INLINE conceIf #-} 
conceIf :: forall w m cx cp cy.
  (Get w m cx
  , Get w m cp
  , Members w m cx
  , Set w m cy
  , MonadIO m)
  => (cp -> Bool)
  -> (cx -> cy)
  -> SystemT w m ()
conceIf cond f =  do
  sx :: Storage cx <- getStore
  sp :: Storage cp <- getStore
  sy :: Storage cy <- getStore
  lift $ do
    sl <- explMembers (sx,sp)
    es <- U.filterM (\n -> (explGet sp n) >>= return . cond) sl
    if (U.null es) then return () else do
      e <- return $ U.head es
      explGet sx e >>=  explSet sy e . f


conceIfM_ :: forall w m cx cp.
  (Get w m cx
  , Get w m cp
  , Members w m cx
  , MonadIO m)
  => (cp -> Bool)
  -> (cx -> SystemT w m ())
  -> SystemT w m ()
conceIfM_ cond f =  do
  sx :: Storage cx <- getStore
  sp :: Storage cp <- getStore
  sl <- lift $ explMembers (sx,sp)
  {--U.foldM_ (\b e -> do
               if b then do
                 p <- lift (explGet sp e)
                 if cond p then do
                   lift (explGet sx e) >>= f
                   return False
                   else return True
                 else return True
           )
    True sl
  return ()--}
  es <- filter (\(_,p) -> cond p) <$> forM (U.toList sl)
    (\e -> do
        p <- lift $ explGet sp e
        return (e,p)
    )
  when ( not $ null es ) $ do
    e <- lift $ explGet sx ( fst $ head es )
    f e

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

type FindElem (key :: k) (ts :: [k]) = FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) = if i == findElem @t @ts
                             then Just $ unsafeCoerce f
                             else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

match :: forall f ts b. (forall t. f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t

genSingletons [''Actor]

bar :: System World (Maybe Position)
bar = do
  g <- cfoldM (\a b -> return $ b:a) [] :: System World [DebugMode]
  x <- exists (Entity 1) (Proxy :: Proxy Position) --foo (Entity 1) [Proxy :: Proxy Position] :: System World Bool
  if x then return . Just =<< get (Entity 1) else return (Nothing )
  
foo :: forall w m a. (Get w m a, MonadIO m)  => Entity -> [Proxy a] -> SystemT w m a
foo e@(Entity ety) cTypes = do
  
  cTypesNames <- forM cTypes $ \t -> do
    --return $ ConT t
    exists e t
  liftIO . print . head $ cTypesNames
  get e
 -- s  :: Storage a <- getStore
 -- lift $ explExists s ety  
  
