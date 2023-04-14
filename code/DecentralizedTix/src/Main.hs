{-# LANGUAGE OverloadedStrings #-}

module Main where

import           User                   (saveUserPolicy)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  saveUserPolicy "71f450399d81967b635c7d3807017e21892cd79377a85a07d1e8eb82"