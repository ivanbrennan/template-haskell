{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Printf (pr)
import Expr (eval, expr)

main :: IO ()
main =
  do
    putStrLn $(pr "Hello")
    putStrLn ( $(pr "%d") 123 )
    putStrLn ( $(pr "%s") "foo" )

    print $ eval [expr|1|]
    case 1 of
      n -> print n
