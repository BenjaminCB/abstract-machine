module Main where

main :: IO ()
main = print . replicate 3 $ ("Hello, World" :: String)
