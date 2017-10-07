module Main where

import Database
import Config

main :: IO ()
main = do
    config <- readConfig
    print config
    putStrLn "hello world"
