{-# LANGUAGE GADTs, TypeOperators, KindSignatures, MultiParamTypeClasses #-}
-- For ghc 6.6 compatibility
-- {-# OPTIONS -fglasgow-exts -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.DDeepArrow
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
--
-- \"Deep arrows\" as a data type.  Handy for code generation.
----------------------------------------------------------------------

module Data.DDeepArrow
  (
  -- * The DeepArrow data type
    DArrow(..), DVal(..)
  ) where

#if __GLASGOW_HASKELL__ >= 609
import Control.Category
import Prelude hiding ((.), id)
#endif

import Control.Arrow
#if __GLASGOW_HASKELL__ < 610
                      hiding (pure)
#endif

-- haskell-src
import Language.Haskell.Syntax

-- TypeCompose
import Data.Pair (Pair(..))

import Language.Haskell.ToHs
import Control.Arrow.DeepArrow
import Data.FunArr


{----------------------------------------------------------
    The "deep arrow" data type
----------------------------------------------------------}

-- | This GADT mirrors the 'DeepArrow' class and part of the 'FunArr' class.
data DArrow :: * -> * -> * where
 Arr      :: DVal (a -> b) -> a `DArrow` b
 Compose  :: a `DArrow` b -> b `DArrow` c -> a `DArrow` c
 First    :: a `DArrow` a' -> (a,b) `DArrow` (a',b)
 Second   :: b `DArrow` b' -> (a,b) `DArrow` (a,b')
 Result   :: b `DArrow` b' -> (a -> b) `DArrow` (a -> b')
 FunF     :: (c -> a,b) `DArrow` (c -> (a,b))
 FunS     :: (a,c -> b) `DArrow` (c -> (a,b))
 FunR     :: (a -> c -> b) `DArrow` (c -> a -> b)
 CurryA   :: ((a,b) -> c) `DArrow` (a -> b -> c)
 UncurryA :: (a -> b -> c) `DArrow` ((a,b) -> c)
 LAssocA  :: (a,(b,c)) `DArrow` ((a,b),c)
 RAssocA  :: ((a,b),c) `DArrow` (a,(b,c))
 IdA      :: a `DArrow` a
 DupA     :: a `DArrow` (a,a)
 FstA     :: (a,b) `DArrow` a
 SndA     :: (a,b) `DArrow` b
 SwapA    :: (a,b) `DArrow` (b,a)

#if __GLASGOW_HASKELL__ >= 609
instance Category DArrow where
  id  = IdA
  (.) = flip Compose
#endif

instance Arrow DArrow where
  arr      = error "no arr/pure for DDeepArrow"
#if __GLASGOW_HASKELL__ < 609
  (>>>)    = Compose
#endif
  first    = First
  second   = Second

instance DeepArrow DArrow where
  result   = Result
  funF     = FunF
  funS     = FunS
  funR     = FunR
  curryA   = CurryA
  uncurryA = UncurryA
  lAssocA  = LAssocA
  rAssocA  = RAssocA
  idA      = IdA
  dupA     = DupA
  fstA     = FstA
  sndA     = SndA
  swapA    = SwapA

-- | A GADT alternative to terms.  Allows generation of Haskell terms and,
-- from there, strings and eval. 
data DVal :: * -> * where
  ExpDV  :: HsExp -> DVal a
  AppDA  :: a `DArrow` b -> DVal a -> DVal b
  PairDV :: DVal a -> DVal b -> DVal (a,b)

instance Pair DVal where pair = PairDV

instance ToHsExp (DArrow a b) where
  toHsExp (Arr dvFun)     = toHsExp dvFun
  toHsExp (Compose ab bc) = toHsInfix (HsSymbol ">>>") ab bc
  toHsExp (First  f)      = toHsApp1 "first"  f
  toHsExp (Second f)      = toHsApp1 "second" f
  toHsExp (Result f)      = toHsApp1 "result" f
  toHsExp FunF            = varid "funF"
  toHsExp FunS            = varid "funS"
  toHsExp FunR            = varid "funR"
  toHsExp CurryA          = varid "curryA"
  toHsExp UncurryA        = varid "uncurryA"
  toHsExp LAssocA         = varid "lAssocA"
  toHsExp RAssocA         = varid "rAssocA"
  toHsExp IdA             = varid "idA"
  toHsExp DupA            = varid "dupA"
  toHsExp FstA            = varid "fstA"
  toHsExp SndA            = varid "sndA"
  toHsExp SwapA           = varid "swapA"

instance ToHsExp (DVal a) where
  toHsExp (ExpDV expr)  = expr
  toHsExp (AppDA ar dv) = toHsExp ar `HsApp` toHsExp dv
                          -- toHsInfix (HsSymbol "$$") ar dv
  toHsExp (PairDV a b)  = HsTuple [toHsExp a, toHsExp b]


instance FunArr DArrow DVal where
  toArr    = Arr
  IdA $$ v = v
  ar  $$ v = AppDA ar v


instance Show (DArrow a b) where show = prettyAsHsExp
instance Show (DVal a)     where show = prettyAsHsExp

-- instance H.Eval DVal       where eval = compileAsHsExp
