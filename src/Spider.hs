{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}
module Spider where

import Network.Wreq.Session
import Network.Wreq (responseBody)
import Rules
import Database.Migration (Page(..))
import Control.Monad.Logger
import Control.Monad.Reader
import Text.XML (Document)
import Text.HTML.DOM (parseLBS)
import Lens.Micro
import Control.Exception.Lifted
import Control.Monad.Trans.Control
import Control.Concurrent.Lifted (threadDelay)
import qualified Data.Text as T
import Lenses

class HasSession a where
    getSession :: a -> Session

instance HasSession Session where
    getSession = id

catchAll :: (MonadBaseControl IO m) => m a -> (SomeException -> m a) -> m a
catchAll = catch

retry :: (MonadLogger m, MonadBaseControl IO m) => Int -> m a -> m a
retry 0 f = error "exhausted retries"
retry n f = doRetry baseDelay 0
    where
        baseDelay = 1 * (10 ^ 6)
        doRetry delay r | r >= n = error "exhausted retries"
                        | otherwise = catchAll f (caught r (delay + (r * (10 ^ 6))))
        caught r delay e = do
            $(logWarn) (T.pack $ show e)
            $(logWarn) (T.pack $ "Delaying for " ++ (show (delay `div` (10 ^ 6))) ++ " seconds")
            threadDelay delay
            doRetry delay (r+1)

fetchDocument :: (HasSession r, MonadReader r m, MonadLogger m, MonadIO m) => String -> m Document
fetchDocument url = do
    session <- asks getSession
    r <- liftIO $ get session url
    return $ parseLBS (r ^. responseBody)

spiderPage :: (HasSession r, MonadReader r m, MonadLogger m, MonadIO m, MonadBaseControl IO m) => Rule -> String -> m Page
spiderPage (Rule c parser) url = do
    doc <- retry 5 (fetchDocument url)
    return $ parser c doc
