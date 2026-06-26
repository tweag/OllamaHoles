{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

demo :: Int -> Int
demo = _llmFun

main :: IO ()
main = pure ()
