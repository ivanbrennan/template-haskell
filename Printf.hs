{-# LANGUAGE TemplateHaskell #-}

module Printf where

import Language.Haskell.TH (Q, Exp, stringE)

data Format = Literal String

parse :: String -> Format
parse s = Literal s

gen :: Format -> Q Exp
gen (Literal s) = stringE s

pr :: String -> Q Exp
pr s = gen (parse s)
