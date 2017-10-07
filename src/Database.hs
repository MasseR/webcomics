{-# Language TemplateHaskell #-}
{-# Language QuasiQuotes #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleContexts #-}
{-# Language RecordWildCards #-}
{-# Language ConstraintKinds #-}
{-# Language FlexibleInstances #-}
module Database
    ( runMigration
    , withPostgresqlPool ) where

import Database.Persist.TH
import Database.Persist
import Database.Persist.Postgresql (SqlBackend)
import qualified Database.Persist.Postgresql as P
import Data.Text (Text)
import Control.Monad.Trans
import Control.Monad.Reader
import Config
import Data.ByteString.Char8 (pack, ByteString)
import Data.Pool (Pool, withResource)
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Database.Migration

class HasBackend a where
    getBackend :: a -> Pool SqlBackend

instance HasBackend (Pool SqlBackend) where
    getBackend = id

createConnectionString :: PostgresConfig -> ByteString
createConnectionString PostgresConfig{..} =
    pack $ unwords [ "host="++postgresConfigHostname
                   , "port="++(show postgresConfigPort)
                   , "user="++postgresConfigUsername
                   , "password="++postgresConfigPassword]

withPostgresqlPool :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => PostgresConfig -> Int -> (Pool SqlBackend -> m a) -> m a
withPostgresqlPool conf n = P.withPostgresqlPool (createConnectionString conf) n

type DB r m = (HasBackend r, MonadReader r m, MonadBaseControl IO m, MonadIO m)

runMigration :: DB r m => m ()
runMigration = do
    pool <- asks getBackend
    withResource pool $ \sql -> runReaderT (P.runMigration migrateAll) sql
