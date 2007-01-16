{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Tupler
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- The tuple type constructors (i.e., (,), (,,), etc) are to value types,
-- as the /tupler/ type constructors are to type constructors.
----------------------------------------------------------------------

module Data.Tupler
  (
    Pair1(..), Pair2(..)
  ) where


import Data.Typeable  -- or AltData.Typeable ?

-- | Pairing for unary type constructors.
newtype Pair1 f g a = Pair1 {unPair1 :: (f a, g a)}
  deriving (Eq, Ord, Show)

instance (Typeable1 f, Typeable1 g) => Typeable1 (Pair1 f g) where
  typeOf1 = const $ mkTyConApp (mkTyCon tyConStr) []
   where
     tyConStr = "Data.Tupler.Pair1"
                ++ tcStringSP (undefined :: f Bool)
                ++ tcStringSP (undefined :: g Bool)

-- | Pairing for binary type constructors.
newtype Pair2 f g a b = Pair2 {unPair2 :: (f a b, g a b)}
  deriving (Eq, Ord, Show)

instance (Typeable2 f, Typeable2 g) => Typeable2 (Pair2 f g) where
  typeOf2 = const $ mkTyConApp (mkTyCon tyConStr) []
   where
     tyConStr = "Data.Tupler.Pair2"
                ++ tcStringSP (undefined :: f Bool Bool)
                ++ tcStringSP (undefined :: g Bool Bool)


---- Misc

-- | Disambuating hack for a function argument.  Wrap parens around a string
-- if it contains a space.
parensIfSpace :: String -> String
parensIfSpace str | ' ' `elem` str = "("++str++")"
                  | otherwise      = str

-- | Extract the type constructor as a string
tcString :: Typeable a => a -> String
tcString = tyConString . typeRepTyCon . typeOf

-- | 'tcString' with disambiguating optional parens
tcStringP :: Typeable a => a -> String
tcStringP = parensIfSpace . tcString

-- | 'tcString' with leading space and optional parens
tcStringSP :: Typeable a => a -> String
tcStringSP = (' ':) . tcStringP
