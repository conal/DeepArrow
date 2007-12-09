{-# LANGUAGE TypeSynonymInstances #-}
----------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.ToHs
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Convert values to Haskell abstract syntax
----------------------------------------------------------------------

module Language.Haskell.ToHs
  (
    ToHsExp(..), varid, varsym
  , toHsApp1, toHsApp2, infixApp, toHsInfix
  , prettyAsHsExp
  ) where

-- haskell-src
import Language.Haskell.Syntax

-- DeepArrow
import Language.Haskell.Parens (pretty)

-- -- | Phantomly Typed Haskell expression
-- data HsExpr a = HsExp

-- | Conversion to Haskell expressions
class ToHsExp a where toHsExp :: a -> HsExp

instance ToHsExp Char   where toHsExp = HsLit . HsCharPrim
instance ToHsExp String where toHsExp = HsLit . HsStringPrim
instance ToHsExp Int    where toHsExp = HsLit . HsIntPrim   . toInteger
instance ToHsExp Float  where toHsExp = HsLit . HsFloatPrim . toRational
instance ToHsExp Double where toHsExp = HsLit . HsFloatPrim . toRational
instance ToHsExp Bool   where toHsExp = varid . show

varid :: String -> HsExp
varid = HsVar . UnQual . HsIdent

varsym :: String -> HsExp
varsym = HsVar . UnQual . HsSymbol

toHsApp1 :: ToHsExp a => String -> a -> HsExp
toHsApp1 name a = varid name `HsApp` toHsExp a

toHsApp2 :: (ToHsExp a, ToHsExp b) => String -> a -> b -> HsExp
toHsApp2 name a b = toHsApp1 name a `HsApp` toHsExp b

infixApp :: HsName -> HsExp -> HsExp -> HsExp
infixApp op l r = HsParen (HsInfixApp l (HsQVarOp (UnQual op)) r)

toHsInfix :: (ToHsExp a, ToHsExp b) => HsName -> a -> b -> HsExp
toHsInfix name a b = infixApp name (toHsExp a) (toHsExp b)

prettyAsHsExp :: ToHsExp a => a -> String
prettyAsHsExp = pretty . toHsExp


-- compileAsHsExp :: ToHsExp (k a) => Ty a -> k a -> a
-- compileAsHsExp tya e =
--   compileD tya (error "compile: no default") (pretty (toHsExp e))
