{-# LANGUAGE TemplateHaskell #-}

module Printf where

import Language.Haskell.TH (Q, Exp, stringE)

pr :: String -> Q Exp
pr s = stringE s
