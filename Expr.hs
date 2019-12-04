{-# LANGUAGE DeriveDataTypeable #-}

module Expr where

import Data.Data (Data)
import Language.Haskell.TH (Q, Exp(LitE), Lit(IntegerL), Pat, Dec, Type)
import Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter),
  quoteExp, quotePat, quoteDec, quoteType, dataToExpQ, dataToPatQ)

data Expr
  = IntExpr Integer
  deriving (Show, Data)

parseExpr :: String -> Q Expr
parseExpr s =
  pure $ IntExpr (read s)

eval :: Expr -> Integer
eval (IntExpr n) = n

expr :: QuasiQuoter
expr =
  QuasiQuoter
    { quoteExp  = quoteExprExp
    , quotePat  = quoteExprPat
    , quoteDec  = quoteExprDec
    , quoteType = quoteExprType
    }

quoteExprExp :: String -> Q Exp
quoteExprExp s =
  parseExpr s >>= dataToExpQ (const Nothing)

quoteExprPat :: String -> Q Pat
quoteExprPat s =
  parseExpr s >>= dataToPatQ (const Nothing)

quoteExprType :: String -> Q Type
quoteExprType _ = fail "quoteExprType not implemented"

quoteExprDec :: String -> Q [Dec]
quoteExprDec _ = fail "quoteExprDec not implemented"
