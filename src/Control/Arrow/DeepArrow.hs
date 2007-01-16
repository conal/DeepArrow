{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.DeepArrow
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
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
  -- * Some utilities
  , (->|)
  ) where

import Control.Arrow

import Data.Tupler (Pair2(..))
import Data.FunArr


{----------------------------------------------------------
    The "deep arrow class"
----------------------------------------------------------}

{- | 

Arrows for deep application.  Most of these methods could be defined
using 'arr', but 'arr' is not definable for some types.  If your
'DeepArrow' instance has 'arr', you might want to use these
implementations

@
    'idA'      = 'arr' 'id'
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
    'second' f = 'swapA' '>>>' 'first' f '>>>' 'swapA'
    f '&&&' g  = 'dupA'  '>>>' f '***' g
@

In a few cases, there are default methods, as noted below.  The
defaults do not use 'arr'.

-}

class Arrow (~>) => DeepArrow (~>) where
  -- | Apply arrow in a function's result.  Analogous to 'first' and
  -- 'second'.
  result :: (b ~> b') -> ((a->b) ~> (a->b'))
  -- Complicates OFun considerably and not used.
  -- Apply arrow in a function's argument.  Note contravariance.
  -- argument :: (a' ~> a ) -> ((a->b) ~> (a'->b))
  -- | Identity.
  idA :: a ~> a
  -- | Duplicate.
  dupA :: a ~> (a,a)
  -- | Extract first.
  fstA :: (a,b) ~> a
  -- | Extract second.
  sndA :: (a,b) ~> b
  -- | Extract function from first element.
  funF :: (c->a, b) ~> (c->(a,b))
  -- | Extract function from second element.
  funS :: (a, c->b) ~> (c->(a,b))  -- Could default via swapA & funF
  -- | Extract function from result.
  funR :: (a->c->b) ~> (c->a->b)
  -- | Curry arrow.
  curryA :: ((a,b)->c) ~> (a->b->c)
  -- | Uncurry arrow.
  uncurryA :: (a->b->c)  ~> ((a,b)->c)
  -- | Swap elements.  Has default.
  swapA :: (a,b) ~> (b,a)
  swapA = sndA &&& fstA
  -- | Left-associate.  Has default.
  lAssocA :: (a,(b,c)) ~> ((a,b),c)
  lAssocA = (idA***fstA) &&& (sndA>>>sndA)
  -- | Right-associate.  Has default.
  rAssocA :: ((a,b),c) ~> (a,(b,c))
  rAssocA = (fstA>>>fstA) &&& (sndA *** idA)
  -- I don't think this one is used.
  -- composeA :: Arrow (~~>) => (a ~~> b, b ~~> c) ~> (a ~~>c)
  -- composeA = arr (uncurry (>>>))


{----------------------------------------------------------
    Composable function extractors
----------------------------------------------------------}

-- | Promote a function extractor into one that reaches into the first
-- element of a pair.
funFirst   :: DeepArrow (~>) => (d ~> (c->a)) -> ((d, b) ~> (c->(a, b)))

-- | Promote a function extractor into one that reaches into the second
-- element of a pair.
funSecond  :: DeepArrow (~>) => (d ~> (c->b)) -> ((a, d) ~> (c->(a, b)))

-- | Promote a function extractor into one that reaches into the result
-- element of a function.
funResult  :: DeepArrow (~>) => (d ~> (c->b)) -> ((a->d) ~> (c->(a->b)))

funFirst  h = first  h >>> funF
funSecond h = second h >>> funS
funResult h = result h >>> funR


{----------------------------------------------------------
    Composable input extractors
----------------------------------------------------------}

-- | Extract the first component of a pair input.
inpF :: DeepArrow (~>) => ((a,b) -> c) ~> (a -> (b->c))
inpF = curryA

-- | Extract the second component of a pair input.
inpS :: DeepArrow (~>) => ((a,b) -> c) ~> (b -> (a->c))
inpS = curryA >>> flipA


-- Given a way to extract a @d@ input from an @a@ input, leaving an @a'@
-- residual input, 'inpFirst' yields a way to extract a @d@ input from an
-- @(a,b)@ input, leaving an @(a',b)@ residual input.
inpFirst   ::  DeepArrow (~>) =>
               (( a   ->c) ~> (d -> ( a'   ->c)))
           ->  (((a,b)->c) ~> (d -> ((a',b)->c)))

-- Analogous to 'inpFirst'.
inpSecond  ::  DeepArrow (~>) =>
               ((   b ->c) ~> (d -> (   b' ->c)))
           ->  (((a,b)->c) ~> (d -> ((a,b')->c)))


-- See ICFP submission for the derivation of inpFirst and inpSecond

inpFirst  h =  curryA >>> flipA >>> result h >>> flipA >>>
               result (flipA>>>uncurryA)

inpSecond h =  curryA >>> result h >>> flipA >>>
               result uncurryA


{----------------------------------------------------------
    Misc functions
----------------------------------------------------------}

-- | Flip argument order
flipA :: DeepArrow (~>) => (a->c->b)  ~> (c->a->b)
flipA = funR

-- | Like 'unzip' but for 'DeepArrow' arrows instead of lists.
unzipA :: DeepArrow (~>) => (a ~> (b,c)) -> (a ~> b, a ~> c)
unzipA f = (f >>> fstA, f >>> sndA)


{----------------------------------------------------------
    Some 'DeepArrow' instances
----------------------------------------------------------}

instance DeepArrow (->) where
  result   = (.)
  -- argument = flip (.)
  -- Since (->) implements 'arr', use the recommended defaults for the rest.
  idA      = arr id
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


-- Arrow "pairs" are arrows
instance (Arrow f, Arrow f') => Arrow (Pair2 f f') where
  arr h                         = Pair2 (arr h   , arr h    )
  Pair2 (f,f') >>> Pair2 (g,g') = Pair2 (f>>>g   , f'>>>g'  )
  first  (Pair2 (f,f'))         = Pair2 (first f , first f' )
  second (Pair2 (f,f'))         = Pair2 (second f, second f')
  Pair2 (f,f') *** Pair2 (g,g') = Pair2 (f***g   , f'***g'  )
  Pair2 (f,f') &&& Pair2 (g,g') = Pair2 (f&&&g   , f'&&&g'  )

-- and DeepArrow "pairs" are deep arrows

instance (DeepArrow ar, DeepArrow ar') => DeepArrow (Pair2 ar ar') where
  idA      = Pair2 (idA,      idA)
  dupA     = Pair2 (dupA,     dupA)
  fstA     = Pair2 (fstA,     fstA)
  sndA     = Pair2 (sndA,     sndA)
  funF     = Pair2 (funF,     funF)
  funS     = Pair2 (funS,     funS)
  funR     = Pair2 (funR,     funR)
  curryA   = Pair2 (curryA,   curryA)
  uncurryA = Pair2 (uncurryA, uncurryA)
  swapA    = Pair2 (swapA,    swapA)
  lAssocA  = Pair2 (lAssocA,  lAssocA)
  rAssocA  = Pair2 (rAssocA,  rAssocA)

  result   (Pair2 (f,f')) = Pair2 (result   f, result   f')

  -- composeA = Pair2 (composeA, composeA)
  -- argument (Pair2 (f,f')) = Pair2 (argument f, argument f')


{----------------------------------------------------------
    Some utilities
----------------------------------------------------------}

-- | Compose wrapped functions
(->|) :: (DeepArrow (~>), FunArr (~>) w) =>
        w (a->b) -> w (b->c) -> w (a->c)
(->|) f g = result (toArr g) $$ f

