{-# LANGUAGE OverloadedStrings #-}

module Main where

import           User                   (saveUserPolicy)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  saveUserPolicy "a71860d5e0b35967e9218a49d227cc460a1ee5b2d86f7d6cfb051ba9"