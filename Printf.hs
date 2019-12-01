{-# LANGUAGE TemplateHaskell #-}

module Printf where

import Language.Haskell.TH (Q, Exp, stringE)

data Format = D | Literal String

parse :: String -> Format
parse "%d" = D
parse s    = Literal s

gen :: Format -> Q Exp
gen D           = [| \n -> show n |]
gen (Literal s) = stringE s

pr :: String -> Q Exp
pr s = gen (parse s)
