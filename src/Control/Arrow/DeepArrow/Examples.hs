{-# LANGUAGE TypeOperators #-}
-- For ghc 6.6 compatibility
-- {-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.DeepArrow.Examples
-- Copyright   :  (c) Conal Elliott 2007-2013
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- DeepArrow examples.
-- 
-- The types in the source code are formatted for easier reading.
----------------------------------------------------------------------

module Control.Arrow.DeepArrow.Examples
  (
  -- * Deep application
    deep
  -- * Function extraction
  , extF, extFF
  -- * Input extraction
  , extI, extFI
  ) where

import Prelude ()
-- import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.DeepArrow


{----------------------------------------------------------
    Deep application    
----------------------------------------------------------}

-- | Given a value of type @(a -> (f,b -> (c,g)),e)@, apply a function to
-- just the @c@ part and leave the rest intact.
-- 
-- @deep = 'first' . 'result' . 'second' . 'result' . 'first' @
deep :: DeepArrow ar => (c `ar` c') ->
          (a -> (f,b -> (c ,g)),e)
     `ar` (a -> (f,b -> (c',g)),e)
deep = first.result.second.result.first



{----------------------------------------------------------
    Function extraction
----------------------------------------------------------}

-- | Given a way to extract a function from a @d@ value, create a way to
-- extract a function from a @(e -> (a,d), f)@ value.
-- 
-- @extF = 'funFirst' . 'funResult' . 'funSecond'@

extF :: DeepArrow ar => (d `ar` (c -> b)) ->
                (e -> (a,d), f)
     `ar` (c -> (e -> (a,b), f))
extF = funFirst.funResult.funSecond

-- | To make an extractor, simply apply the extractor-transformer 'extF'
-- to the identity arrow.
-- 
-- @'extFF' = 'extF' 'idA'@
extFF :: DeepArrow ar =>
                 (e -> (a,(c-> b)),f)
      `ar` (c -> (e -> (a,     b),f))
extFF = extF id


{----------------------------------------------------------
    Input extraction
----------------------------------------------------------}

-- | Extract a @b@ input from a @((a,(b,e)),c)@ argument.
-- 
-- @extI = ('inpFirst' . 'inpSecond') 'inpF'@
extI :: DeepArrow ar =>
          (     ((a,(b,e)),c) -> d)
     `ar` (b -> ((a,   e ),c) -> d)
extI = (inpFirst.inpSecond) inpF


-- | Typically, we will have to combine function and input extractors.
-- For instance, combine 'extF' and 'extI'.
-- 
-- @extFI = 'extF' 'extI'@
extFI :: DeepArrow ar =>
                 (e -> (g,(((a,(b,e)),c) -> d)), f)
      `ar` (b -> (e -> (g,(((a,   e) ,c) -> d)), f))
extFI = extF extI
