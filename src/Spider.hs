{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}
{-# Language ConstraintKinds #-}
{-# Language OverloadedStrings #-}
module Spider where

import Network.Wreq.Session
import Network.Wreq (responseBody)
import Rules
import Database.Migration (Page(..), EntityField(..))
import Database
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
import Network.URI
import Data.Monoid
import Data.Maybe (isJust)

class HasSession a where
    getSession :: a -> Session

instance HasSession Session where
    getSession = id

type Spider r m = (HasSession r, MonadReader r m, MonadLogger m, MonadIO m, MonadBaseControl IO m)

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

fetchDocument :: Spider r m => String -> m Document
fetchDocument url = do
    $(logInfo) (T.pack $ "Fetching " ++ url)
    session <- asks getSession
    r <- liftIO $ get session url
    return $ parseLBS (r ^. responseBody)

parsePage :: Spider r m => Rule -> String -> m Page
parsePage (Rule c parser) url = do
    doc <- retry 5 (fetchDocument url)
    return $ parser c doc

traverseBack :: (Spider r m, DB r m) => Rule -> m ()
traverseBack rule@(Rule base _) = go base
    where
        true = const True
        goBack url Nothing = $(logInfo) "No previous url"
        goBack url (Just previous) = do
            let previousURI = relativeTo <$> parseURIReference (T.unpack previous) <*> parseURIReference (T.unpack url)
            case previousURI of
                 Nothing -> $(logWarn) "Could not parse url"
                 Just u -> go (T.pack $ show u)
        go url = do
            $(logInfo) "Fetching"
            page <- parsePage rule (T.unpack url)
            prev <- selectFirst [PagePrevious ==. (page ^. previous)] []
            next <- selectFirst [PagePrevious ==. (page ^. previous)] []
            let alreadySeen = maybe False id $ (&&) <$> (fmap true next) <*> (fmap true prev)
            unless alreadySeen $ do
                insert page
                threadDelay (1 * (10^6))
                goBack url (page ^. previous)
