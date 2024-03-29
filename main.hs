{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Printf (pr)
import Expr (Expr(IntExpr), eval, expr)

main :: IO ()
main =
  do
    putStrLn $(pr "Hello")
    putStrLn ( $(pr "%d") 123 )
    putStrLn ( $(pr "%s") "foo" )

    print $ eval [expr|1 + 2|]
    print $ eval [expr|1 - 2|]
    case IntExpr 1 of
      [expr|'int:n|] -> print n
      _ -> pure ()
