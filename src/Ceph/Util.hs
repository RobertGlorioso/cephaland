{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict #-}
module Ceph.Util where

import Ceph.Components

import Control.Monad.Reader
import Linear
import Apecs
import Apecs.Core
import Data.Group
import Data.Vector.Unboxed as U
--import GHC.Prim

v2ToRad :: (Floating a, Ord a) => V2 a -> a
v2ToRad (V2 m n) = case compare m 0 of
  LT -> atan ( n / m ) + pi
  GT -> atan ( n / m ) 
  EQ -> (-pi)/2 * (signum n)

conceM_ :: forall w m cx. (Get w m cx, Members w m cx) => (cx -> SystemT w m ()) -> SystemT w m ()
conceM_ f =  do
  sx :: Storage cx <- getStore
  sl <- lift $ explMembers sx
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

  --sl <- lift $ explMembers (sx,sp)
  --e <- U.head <$> U.filterM (\n -> lift (explGet sp n ) >>= return . not . cond) sl
  --lift $ explGet sx e >>= explSet sy e . f

conceIfM_ :: forall w m cx cp.
  (Get w m cx
  , Get w m cp
  , Members w m cx)
  => (cp -> Bool)
  -> (cx -> SystemT w m ())
  -> SystemT w m ()
conceIfM_ cond f =  do
  sx :: Storage cx <- getStore
  sp :: Storage cp <- getStore
  sl <- lift $ explMembers (sx,sp)
  es <- lift $ U.filterM (\e -> explGet sp e >>= return . cond) sl
    --guard (U.null es)
  when (not $ U.null es) $ do
    e <- lift $ explGet sx ( U.head es )
    f e
    --p <- explGet sp $ U.head e
    --when (cond p) $ do
