{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.DeepArrow
-- Copyright   :  (c) Conal Elliott 2006-2013
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
--
-- \"Deep arrows\" as an 'Arrow' subclass.
----------------------------------------------------------------------

module Control.Arrow.DeepArrow
  (
  -- * The DeepArrow class
   DeepArrow(..)
  -- * Composable function extractors
  , funFirst, funSecond, funResult
  -- * Composable input extractors
  , inpF, inpS, inpFirst, inpSecond
  -- * Misc functions
  , flipA, unzipA
  -- * 'DeepArrow' instance helper
  , FunDble(..)
  -- * Some utilities
  , (->|)
  ) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow

import Control.Compose ((::*::)(..),(*::*),inProdd,FunA(..),inFunA, FunAble)

import Data.FunArr


{----------------------------------------------------------
    The "deep arrow" class
----------------------------------------------------------}

{- | 

Arrows for deep application.  Most of these methods could be defined
using 'arr', but 'arr' is not definable for some types.  If your
'DeepArrow' instance has 'arr', you might want to use these
implementations

@
    'fstA'     = 'arr' 'fst'
    'dupA'     = 'arr' (\\ x -> (x,x))
    'sndA'     = 'arr' 'snd'
    'funF'     = 'arr' (\\ (f,b) -> \\ c -> (f c, b))
    'funS'     = 'arr' (\\ (a,f) -> \\ c -> (a, f c))
    'funR'     = 'arr' 'flip'
    'curryA'   = 'arr' 'curry'
    'uncurryA' = 'arr' 'uncurry'
    'swapA'    = 'arr' (\\ (a,b) -> (b,a))
    'lAssocA'  = 'arr' (\\ (a,(b,c)) -> ((a,b),c))
    'rAssocA'  = 'arr' (\\ ((a,b),c) -> (a,(b,c)))
@

If your 'DeepArrow' instance /does not/ have 'arr', you'll have to come up
with other definitions.  In any case, I recommend the following
definitions, which mirror 'Arrow' defaults while avoiding 'arr'.  Be sure
also to define 'arr' or 'pure' to yield an error message (rather than
ping-ponging infinitely between them via the 'Arrow' default definitions).

@
    'second' f = 'swapA' '.' 'first' f '.' 'swapA'
    f '&&&' g  = 'dupA'  '>>>' f '***' g
@

In a few cases, there are default methods, as noted below.  The
defaults do not use 'arr'.

-}

class Arrow ar => DeepArrow ar where
  -- | Direct arrow into a function's result.  Analogous to 'first' and
  -- 'second'.
  result :: (b `ar` b') -> ((a->b) `ar` (a->b'))
  -- Complicates OFun considerably and not used.
  -- Direct arrow into a function's argument.  Note contravariance.
  -- argument :: (a' `ar` a ) -> ((a->b) `ar` (a'->b))
--   -- | Identity. Unnecessary now that we have Category 
--   idA :: a `ar` a
--   idA = id
  -- | Duplicate.
  dupA :: a `ar` (a,a)
  -- | Extract first.
  fstA :: (a,b) `ar` a
  -- | Extract second.
  sndA :: (a,b) `ar` b
  -- | Extract function from first element.
  funF :: (c->a, b) `ar` (c->(a,b))
  -- | Extract function from second element.
  funS :: (a, c->b) `ar` (c->(a,b))  -- Could default via swapA & funF
  -- | Extract function from result.
  funR :: (a->c->b) `ar` (c->a->b)
  -- | Curry arrow.
  curryA :: ((a,b)->c) `ar` (a->b->c)
  -- | Uncurry arrow.
  uncurryA :: (a->b->c)  `ar` ((a,b)->c)
  -- | Swap elements.  Has default.
  swapA :: (a,b) `ar` (b,a)
  swapA = sndA &&& fstA
  -- | Left-associate.  Has default.
  lAssocA :: (a,(b,c)) `ar` ((a,b),c)
  lAssocA = second fstA &&& (sndA . sndA)
  -- | Right-associate.  Has default.
  rAssocA :: ((a,b),c) `ar` (a,(b,c))
  rAssocA = (fstA . fstA) &&& first  sndA
  -- I don't think this one is used.
  -- composeA :: Arrow (~`ar`) => (a ~`ar` b, b ~`ar` c) `ar` (a ~`ar`c)
  -- composeA = arr (uncurry (>>>))


{----------------------------------------------------------
    Composable function extractors
----------------------------------------------------------}

-- | Promote a function extractor into one that reaches into the first
-- element of a pair.
funFirst   :: DeepArrow ar => (a `ar` (d->a')) -> ((a,b) `ar` (d->(a',b)))

-- | Promote a function extractor into one that reaches into the second
-- element of a pair.
funSecond  :: DeepArrow ar => (b `ar` (d->b')) -> ((a,b) `ar` (d->(a,b')))

-- | Promote a function extractor into one that reaches into the result
-- element of a function.
funResult  :: DeepArrow ar => (b `ar` (d->b')) -> ((a->b) `ar` (d->(a->b')))

funFirst  h = funF . first  h
funSecond h = funS . second h
funResult h = funR . result h


{----------------------------------------------------------
    Composable input extractors
----------------------------------------------------------}

-- | Extract the first component of a pair input.
inpF :: DeepArrow ar => ((a,b) -> c) `ar` (a -> (b->c))
inpF = curryA

-- | Extract the second component of a pair input.
inpS :: DeepArrow ar => ((a,b) -> c) `ar` (b -> (a->c))
inpS = flipA . curryA


-- | Given a way to extract a @d@ input from an @a@ input, leaving an @a'@
-- residual input, 'inpFirst' yields a way to extract a @d@ input from an
-- @(a,b)@ input, leaving an @(a',b)@ residual input.
inpFirst   ::  DeepArrow ar =>
               (( a   ->c) `ar` (d -> ( a'   ->c)))
           ->  (((a,b)->c) `ar` (d -> ((a',b)->c)))

-- | Analogous to 'inpFirst'.
inpSecond  ::  DeepArrow ar =>
               ((   b ->c) `ar` (d -> (   b' ->c)))
           ->  (((a,b)->c) `ar` (d -> ((a,b')->c)))


-- See ICFP 07 paper for the derivation of inpFirst and inpSecond

inpFirst  h = result (uncurryA . flipA) . flipA . result h . flipA . curryA 

inpSecond h = result uncurryA . flipA . result h . curryA 


{----------------------------------------------------------
    Misc functions
----------------------------------------------------------}

-- | Flip argument order
flipA :: DeepArrow ar => (a->c->b)  `ar` (c->a->b)
flipA = funR

-- | Like 'unzip' but for 'DeepArrow' arrows instead of lists.
unzipA :: DeepArrow ar => (a `ar` (b,c)) -> (a `ar` b, a `ar` c)
unzipA f = (fstA . f, sndA . f)


{----------------------------------------------------------
    Some 'DeepArrow' instances
----------------------------------------------------------}

instance DeepArrow (->) where
  result   = (.)
  -- argument = flip (.)
  -- Since (->) implements 'arr', use the recommended defaults for the rest.
  -- idA      = arr id
  fstA     = arr fst
  dupA     = arr (\x->(x,x))
  sndA     = arr snd
  funF     = arr (\ (f,b) -> \ c -> (f c, b))
  funS     = arr (\ (a,f) -> \ c -> (a, f c))
  funR     = arr flip
  curryA   = arr curry
  uncurryA = arr uncurry
  swapA    = arr (\ (a,b) -> (b,a))
  lAssocA  = arr (\ (a,(b,c)) -> ((a,b),c))
  rAssocA  = arr (\ ((a,b),c) -> (a,(b,c)))

-- DeepArrow "pairs" are deep arrows

instance (DeepArrow ar, DeepArrow ar') => DeepArrow (ar ::*:: ar') where
  dupA     = dupA     *::*     dupA
  fstA     = fstA     *::*     fstA
  sndA     = sndA     *::*     sndA
  funF     = funF     *::*     funF
  funS     = funS     *::*     funS
  funR     = funR     *::*     funR
  curryA   = curryA   *::*   curryA
  uncurryA = uncurryA *::* uncurryA
  swapA    = swapA    *::*    swapA
  lAssocA  = lAssocA  *::*  lAssocA
  rAssocA  = rAssocA  *::*  rAssocA

  result = inProdd (result *** result)

  -- idA      = idA      *::*      idA
  -- composeA = Prodd (composeA, composeA)
  -- argument (Prodd (f,f')) = Prodd (argument f, argument f')


-- | Support needed for a 'FunA' to be a 'DeepArrow' (as 'FunAble' serves
-- 'Arrow').
class FunAble h => FunDble h where
  resultFun   :: (h b -> h b') -> (h (a->b) -> h (a->b'))
  dupAFun     :: h a -> h (a,a)
  fstAFun     :: h (a,b) -> h a
  sndAFun     :: h (a,b) -> h b
  funFFun     :: h (c->a, b) -> h (c->(a,b))
  funSFun     :: h (a, c->b) -> h (c->(a,b))
  funRFun     :: h (a->c->b) -> h (c->a->b)
  curryAFun   :: h ((a,b)->c) -> h (a->b->c)
  uncurryAFun :: h (a->b->c)  -> h ((a,b)->c)
  swapAFun    :: h (a,b) -> h (b,a)
  lAssocAFun  :: h (a,(b,c)) -> h ((a,b),c)
  rAssocAFun  :: h ((a,b),c) -> h (a,(b,c))


instance FunDble h => DeepArrow (FunA h) where
  result   = inFunA resultFun
  -- idA      = FunA id
  dupA     = FunA dupAFun
  fstA     = FunA fstAFun
  sndA     = FunA sndAFun
  funF     = FunA funFFun
  funS     = FunA funSFun
  funR     = FunA funRFun
  curryA   = FunA curryAFun
  uncurryA = FunA uncurryAFun
  swapA    = FunA swapAFun
  lAssocA  = FunA lAssocAFun
  rAssocA  = FunA rAssocAFun

--   (>>>)  = inFunA2 (>>>)
--   first  = inFunA  firstFun
--   second = inFunA  secondFun
--   (***)  = inFunA2 (***%)
--   (&&&)  = inFunA2 (&&&%)




{----------------------------------------------------------
    Some utilities
----------------------------------------------------------}

-- | Compose wrapped functions
(->|) :: (DeepArrow ar, FunArr ar w)
      => w (a->b) -> w (b->c) -> w (a->c)
(->|) f g = result (toArr g) $$ f

-- -- | Pre- and post-processing
-- ar :: DeepArrow (-->) => (a' --> a) -> (b --> b') -> ((a -> b) --> (a' -> b'))
-- f `ar` h = result h . argument f
