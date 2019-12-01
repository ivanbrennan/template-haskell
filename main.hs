{-# LANGUAGE TemplateHaskell #-}

module Main where

import Printf (pr)

main :: IO ()
main = putStrLn $(pr "Hello")
    >> putStrLn ( $(pr "%d") 123 )
    >> putStrLn ( $(pr "%s") "foo" )
