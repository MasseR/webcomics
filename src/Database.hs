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
    , Pool
    , SqlBackend
    , HasBackend(..)
    , withPostgresqlPool
    , insert
    , selectFirst
    , DB
    , Record
    , (P.==.)) where

import Database.Persist.TH
import qualified Database.Persist as P
import qualified Database.Persist as P_EXPORT hiding (insert, selectFirst)
import Database.Persist (PersistEntity, PersistEntityBackend, Filter, SelectOpt, PersistQueryRead, Entity)
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
type Record record = (PersistEntityBackend record ~ SqlBackend, PersistEntity record)

runMigration :: DB r m => m ()
runMigration = do
    pool <- asks getBackend
    withResource pool $ \sql -> runReaderT (P.runMigration migrateAll) sql

insert :: (DB r m, Record record) => record -> m (Key record)
insert record = do
    pool <- asks getBackend
    withResource pool $ \sql -> runReaderT (P.insert record) sql

selectFirst :: (DB r m, Record record) =>
    [Filter record] -> [SelectOpt record] -> m (Maybe (Entity record))
selectFirst f s = do
    pool <- asks getBackend
    withResource pool $ \sql -> runReaderT (P.selectFirst f s) sql
