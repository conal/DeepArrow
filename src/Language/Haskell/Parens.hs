----------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Parens
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  ???
-- 
-- Insert parens where necessary in Haskell expressions.
-- Inspired by Sigbjorn Finne's Pan pretty-printer.
----------------------------------------------------------------------

module Language.Haskell.Parens
  (
   pretty, Cify, cifyExp, unCify, hasOpInfo
  )
    where

-- haskell-src
import Language.Haskell.Syntax
import Language.Haskell.Pretty (prettyPrint)

pretty :: HsExp -> String
pretty = prettyPrint . unCify . cifyExp

-- Associativity
data Assoc = LAssoc | NAssoc | RAssoc deriving ( Eq )

-- Associativity and precedence
type OpInfo = (Assoc, Int)

-- Context in which an expression appears.  Oper info and whether we're on
-- the right.
type Context = (OpInfo, Bool)

type CExp = Context -> HsExp


-- Given an "inner" operator application "a `opi` b", in the context of an
-- application of an outer operator "opo", insert parens around the inner
-- iff
-- 
-- + opo has a higher precedence than opi, or
-- + opo has the same precedence as opi, and either
--   - opo & opi differs in associativity direction, or
--   - opo & opi have the same associativity direction, and either
--     * "a `opi` b" occurs on the right, and opo is not right-assoc, or
--     * "a `opi` b" occurs on the left, and opo is not left-assoc.

oper :: OpInfo -> CExp -> CExp
oper i@(asi,pri) cexp ((aso,pro),onR) = mbParens (cexp c')
 where   -- i=inner, o=outer
  mbParens | needParens = HsParen
           | otherwise  = id
  c' = (i,False)
  needParens = pro > pri || (pro == pri && conflict)
  conflict = aso /= asi || onR /= (aso == RAssoc)

-- Reset context.
reset :: CExp -> CExp
reset = oper (NAssoc,0)

-- Whether the inner application is on the right.
onRight :: Bool -> CExp -> CExp
onRight onR cexp (opi,_) = cexp (opi,onR)

-- Generally handy lifters
lift1 :: (a->b) -> (z->a) -> (z->b)
lift1 = (.)

lift2 :: (a->b->c) -> (z->a) -> (z->b) -> (z->c)
lift2 f h1 h2 c = f (h1 c) (h2 c)

lift3 :: (a->b->c->d) -> (z->a) -> (z->b) -> (z->c) -> (z->d)
lift3 f h1 h2 h3 c = f (h1 c) (h2 c) (h3 c)

-- Make context-dependent version that inserts parens
type Cify a = a -> (Context -> a)

-- Provide initial context.
unCify :: (Context -> a) -> a
unCify = ($ ((NAssoc,0),False))

cifyExp :: Cify HsExp
cifyExp e =
  case e of
    HsVar _ -> const e
    HsCon _ -> const e
    HsLit _ -> const e
    HsLambda loc pats body -> reset $
                              lift1 (HsLambda loc pats) (cifyExp body)
    HsInfixApp el op er -> oper (opInfo (opName op)) $
                           lift2 (flip HsInfixApp op)
                                 (onRight False (cifyExp el))
                                 (onRight True  (cifyExp er))
    HsApp fun arg -> oper (opInfo (HsSymbol "")) $
                     lift2 HsApp 
                           (onRight False (cifyExp fun))
                           (onRight True  (cifyExp arg))
    HsNegApp arg -> oper (opInfo (HsSymbol "-")) $
                    lift1 HsNegApp (onRight True (cifyExp arg))
    HsLet decls body -> reset $
                        lift2 HsLet
                              (cifyDecls decls)
                              (cifyExp body)
    HsIf c a b -> reset $
                  lift3 HsIf (cifyExp c) (cifyExp a) (cifyExp b)

    HsTuple exps -> reset $
                    lift1 HsTuple (cifyExps exps)
    HsParen e' -> cifyExp e'

    -- TODO: fill in other cases

    _ -> error $ "cifyExp: unhandled case " ++ show e

-- Some other syntactic categories.  Many missing productions.

cifyExps :: Cify [HsExp]
cifyExps exps c = map (flip cifyExp c) exps

cifyDecls :: Cify [HsDecl]
cifyDecls decls c = map (flip cifyDecl c) decls

cifyDecl :: Cify HsDecl
cifyDecl (HsPatBind loc pat rhs []) =
  lift1 (\ r -> HsPatBind loc pat r [])
        (cifyRhs rhs)
cifyDecl decl = error $ "cifyDecl: unhandled case " ++ show decl

-- TODO: fill in other cases

cifyRhs :: Cify HsRhs
cifyRhs (HsUnGuardedRhs expr) = lift1 HsUnGuardedRhs (cifyExp expr)
cifyRhs rhs = error $ "cifyRhs: unhandled case " ++ show rhs

-- TODO: fill in other cases

---- Operator info

opQName :: HsQOp -> HsQName
opQName (HsQVarOp qname) = qname
opQName (HsQConOp qname) = qname

opName :: HsQOp -> HsName
opName = getName . opQName

-- Next two swiped from Haskell.Language.Pretty (not exported)
getName :: HsQName -> HsName
getName (UnQual s) = s
getName (Qual _ s) = s
getName (Special HsCons) = HsSymbol ":"
getName (Special HsFunCon) = HsSymbol "->"
getName (Special s) = HsIdent (specialName s)

specialName :: HsSpecialCon -> String
specialName HsUnitCon = "()"
specialName HsListCon = "[]"
specialName HsFunCon = "->"
specialName (HsTupleCon n) = "(" ++ replicate (n-1) ',' ++ ")"
specialName HsCons = ":"


apPrec :: Int
apPrec  = 10

-- Precedences & fixities.  Derived from the prelude.  Add more as
-- desired.  Really the operators should be extensible.
opInfo :: HsName -> OpInfo
opInfo name = case name of
                -- From the Prelude
                HsSymbol ""        -> (LAssoc, apPrec)  -- jux/application
                HsSymbol "."       -> (RAssoc, 9)
                HsSymbol "!!"      -> (LAssoc, 9)
                HsSymbol "^"       -> (RAssoc, 8)
                HsSymbol "^^"      -> (RAssoc, 8)
                HsSymbol "**"      -> (RAssoc, 8)
                HsSymbol "*"       -> (LAssoc, 7)
                HsSymbol "/"       -> (LAssoc, 7)
                HsIdent  "quot"    -> (LAssoc, 7)
                HsIdent  "rem"     -> (LAssoc, 7)
                HsIdent  "div"     -> (LAssoc, 7)
                HsIdent  "mod"     -> (LAssoc, 7)
                HsSymbol ":%"      -> (LAssoc, 7)
                HsSymbol "%"       -> (LAssoc, 7)
                HsSymbol "+"       -> (LAssoc, 6)
                HsSymbol "-"       -> (LAssoc, 6)
                HsSymbol ":"       -> (RAssoc, 5)
                HsSymbol "++"      -> (RAssoc, 5)
                HsSymbol "=="      -> (NAssoc, 4)
                HsSymbol "/="      -> (NAssoc, 4)
                HsSymbol "<"       -> (NAssoc, 4)
                HsSymbol "<="      -> (NAssoc, 4)
                HsSymbol ">="      -> (NAssoc, 4)
                HsSymbol ">"       -> (NAssoc, 4)
                HsIdent  "elem"    -> (NAssoc, 4)
                HsIdent  "notElem" -> (NAssoc, 4)
                HsSymbol "&&"      -> (RAssoc, 3)
                HsSymbol "||"      -> (RAssoc, 2)
                HsSymbol ">>"      -> (LAssoc, 1)
                HsSymbol ">>="     -> (LAssoc, 1)
                HsSymbol "=<<"     -> (RAssoc, 1)
                HsSymbol "$"       -> (RAssoc, 0)
                HsSymbol "$!"      -> (RAssoc, 0)
                HsIdent  "seq"     -> (RAssoc, 0)

                -- From Control.Arrow
                HsSymbol "<+>"     -> (RAssoc, 5)
                HsSymbol "***"     -> (RAssoc, 3)
                HsSymbol "&&&"     -> (RAssoc, 3)
                HsSymbol "+++"     -> (RAssoc, 2)
                HsSymbol "|||"     -> (RAssoc, 2)
                HsSymbol ">>>"     -> (RAssoc, 1)
                HsSymbol "^>>"     -> (RAssoc, 1)
                HsSymbol ">>^"     -> (RAssoc, 1)
                HsSymbol "<<<"     -> (RAssoc, 1)
                HsSymbol "^<<"     -> (RAssoc, 1)
                HsSymbol "<<^"     -> (RAssoc, 1)

                -- Mine
                HsSymbol "*^"      -> (LAssoc, 7)
                HsSymbol "^+^"     -> (LAssoc, 6)

                -- Bail out
                _                  -> (NAssoc, -1) -- no op info

hasOpInfo :: HsName -> Bool
hasOpInfo name = snd (opInfo name) >= 0
