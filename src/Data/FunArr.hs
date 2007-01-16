{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.FunArr
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Conversion between arrow values and wrapped functions.
----------------------------------------------------------------------

module Data.FunArr
  (
  FunArr(..), wapl
  ) where

import Control.Monad.Identity
import Data.Tupler

infixr 0  $$  -- FunArr application

-- | Convert between an arrow value and a \"wrapped function\".  The \"arrow\"
-- doesn't really have to be an arrow.  I'd appreciate ideas for names &
-- uses.
class FunArr (~>) w | (~>)->w, w->(~>) where
  -- | Convert a @w@-wrapped function to an arrow value
  toArr :: w (a->b) -> (a ~> b)
  -- | Apply an arrow to a @w@-wrapped value
  ($$)  :: (a ~> b) -> w a -> w b

-- | Apply a wrapped function to a wrapped value
wapl :: FunArr (~>) w => w (a->b) -> w a -> w b
wapl f a = toArr f $$ a

-- Function/Id instance
instance FunArr (->) Identity where
  toArr = runIdentity
  f $$ ida = return (f (runIdentity ida))
  -- ($$) f = return . f . runIdentity

-- The following instance violates the "coverage condition" and so
-- requires -fallow-undecidable-instances.

instance (FunArr ar w, FunArr ar' w')
      => FunArr (Pair2 ar ar') (Pair1 w w') where
  toArr (Pair1 (f,f'))         = Pair2 (toArr f, toArr f')
  Pair2 (f,f') $$ Pair1 (w,w') = Pair1 (f $$ w, f' $$ w')


