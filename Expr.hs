{-# LANGUAGE DeriveDataTypeable #-}

module Expr where

import Data.Char (isDigit)
import Data.Data (Data, Typeable, cast)
import Language.Haskell.TH (Q, Exp(LitE), Lit(IntegerL), Pat, Dec, Type,
  conP, varP, mkName)
import Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter),
  quoteExp, quotePat, quoteDec, quoteType, dataToExpQ, dataToPatQ)

data Expr
  = IntExpr Integer
  | BinOpExpr BinOp Expr Expr
  | MetaVar String
  deriving (Show, Data)

data BinOp
  = AddOp
  deriving (Show, Data)

parseExpr :: String -> Q Expr
parseExpr s | all isDigit s =
  pure $ IntExpr (read s)
parseExpr "'int:n" =
  pure $ MetaVar "n"
parseExpr (x:' ':'+':' ':y:[]) =
    BinOpExpr <$> pure AddOp <*> parseExpr [x] <*> parseExpr [y]
parseExpr s =
  fail ("parseExpr no parse: " ++ s)

eval :: Expr -> Integer
eval (IntExpr n) = n
eval (BinOpExpr _ x y) = (+) (eval x) (eval y)

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
    parseExpr s >>= dataToPatQ (const Nothing `extQ` antiExprPat)
  where
    extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
    extQ f g a = maybe (f a) g (cast a)

    antiExprPat :: Expr -> Maybe (Q Pat)
    antiExprPat (MetaVar v) = Just $ conP (mkName "IntExpr") [varP (mkName v)]
    antiExprPat _           = Nothing

quoteExprType :: String -> Q Type
quoteExprType _ = fail "quoteExprType not implemented"

quoteExprDec :: String -> Q [Dec]
quoteExprDec _ = fail "quoteExprDec not implemented"
