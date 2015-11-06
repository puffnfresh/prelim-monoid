{-# LANGUAGE NoImplicitPrelude #-}

module Prelim.Data.Monoid(
  Monoid(identity)
) where

import Control.Applicative(Const(Const))
import Data.Proxy(Proxy)
import Prelim.Prelude(Ordering, Maybe(Nothing), Num)
import Prelim.Data.Semigroup(Semigroup)
import qualified Data.Monoid as Monoid(Any, All, Last, First, Product, Sum, Endo, Dual(Dual), mempty)

class Semigroup a => Monoid a where
  identity ::
    a

instance Monoid Ordering where
  identity =
    Monoid.mempty
    
instance Monoid () where
  identity =
    Monoid.mempty

instance Monoid Monoid.Any where
  identity =
    Monoid.mempty
    
instance Monoid Monoid.All where
  identity =
    Monoid.mempty
    
instance Monoid [a] where
  identity =
    Monoid.mempty

instance Semigroup a => Monoid (Maybe a) where
  identity =
    Nothing
    
instance Monoid (Monoid.Last a) where
  identity =
    Monoid.mempty
    
instance Monoid (Monoid.First a) where
  identity =
    Monoid.mempty
    
instance Num a => Monoid (Monoid.Product a) where
  identity =
    Monoid.mempty
    
instance Num a => Monoid (Monoid.Sum a) where
  identity =
    Monoid.mempty
    
instance Monoid (Monoid.Endo a) where
  identity =
    Monoid.mempty
    
instance Monoid a => Monoid (Monoid.Dual a) where
  identity =
    Monoid.Dual identity

instance Monoid b => Monoid (a -> b) where
  identity =
    \_ -> identity
    
instance (Monoid a, Monoid b) => Monoid (a, b) where
  identity =
    (identity, identity)
    
instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c) where
  identity =
    (identity, identity, identity)
    
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a, b, c, d) where
  identity =
    (identity, identity, identity, identity)
    
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monoid (a, b, c, d, e) where
  identity =
    (identity, identity, identity, identity, identity)
    
instance Monoid (Proxy s) where
  identity =
    Monoid.mempty
    
instance Monoid a => Monoid (Const a b) where
  identity =
    Const identity

-- instance Alternative f => Monoid (Monoid.Alt f a) where
--   identity =
--     Monoid.Alt empty
