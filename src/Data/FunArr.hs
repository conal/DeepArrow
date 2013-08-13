{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies
           , UndecidableInstances
  #-}
-- For ghc 6.6 compatibility
-- {-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.FunArr
-- Copyright   :  (c) Conal Elliott 2007-2013
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Conversion between arrow values and wrapped functions.
----------------------------------------------------------------------

module Data.FunArr
  (
  FunArr(..) -- , wapl
  ) where

-- import Control.Monad.Identity

import Control.Compose

infixr 0  $$  -- FunArr application

-- | Convert between an arrow value and a \"wrapped function\".  The \"arrow\"
-- doesn't really have to be an arrow.  I'd appreciate ideas for names &
-- uses.
class FunArr ar w | ar->w , w->ar where
  -- | Convert a @w@-wrapped function to an arrow value
  toArr :: w (a->b) -> (a `ar` b)
  -- | Apply an arrow to a @w@-wrapped value
  ($$)  :: (a `ar` b) -> w a -> w b

-- -- | Apply a wrapped function to a wrapped value
-- wapl :: FunArr ar w => w (a->b) -> w a -> w b
-- wapl f a = toArr f $$ a

-- The type of wapl matches <*> from Control.Applicative.  What about
-- "pure" from the Applicative class, with type a -> w a?

-- Function/Id instance
instance FunArr (->) Id where
  toArr (Id f) = f
  f $$ Id a    = Id (f a)

-- -- Oops!  This instance can't work with the mutual functional
-- dependencies.  Instead, instantiate it per @h@.
-- 
-- instance FunArr (FunA h) h where
--   toArr = error "toArr: undefined for FunArr" --  Add FunArrable class & delegate
--   FunA f $$ ha = f ha


-- The following instance violates the "coverage condition" and so
-- requires -fallow-undecidable-instances.

instance (FunArr ar w, FunArr ar' w')
    => FunArr (ar ::*:: ar') (w :*: w') where
  toArr (Prod (f,f'))         = Prodd (toArr f, toArr f')
  Prodd (f,f') $$ Prod (w,w') = Prod (f $$ w, f' $$ w')


