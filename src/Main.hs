module Main where

import Database
import Config
import Lenses
import Lens.Micro
import Control.Monad.Logger
import Control.Monad.Reader

main :: IO ()
main = do
    config <- readConfig
    runStdoutLoggingT $ withPostgresqlPool (config ^. postgres) 5 $ \pool ->
        runReaderT runMigration pool
    putStrLn "hello world"
