{-# Language TemplateHaskell #-}
module Main where

import Database
import Config
import Lenses
import Lens.Micro
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad (forM_)
import Rules
import Spider
import Network.Wreq.Session (withSession, Session)
import App
import Control.Concurrent.Async.Lifted


main :: IO ()
main = do
    config <- readConfig
    withSession $ \session ->
        runStdoutLoggingT $ withPostgresqlPool (config ^. postgres) 5 $ \pool -> do
            let app = App pool session
            runReaderT runMigration pool
            runReaderT (mapConcurrently traverseBack rules) app
    putStrLn "hello world"
