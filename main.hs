{-# LANGUAGE TemplateHaskell #-}

module Main where

import Printf (pr)

main :: IO ()
main =
  do
    putStrLn $(pr "Hello")
    putStrLn ( $(pr "%d") 123 )
    putStrLn ( $(pr "%s") "foo" )

    print $ 1
    case 1 of
      n -> print n
