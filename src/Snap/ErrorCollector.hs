{-# LANGUAGE RecordWildCards #-}
module Snap.ErrorCollector
  ( collectErrors
  , LoggedException(..)
  , ErrorCollectorConfig(..)
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (forever, mplus, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (unfoldM')
import Data.Sequence as Seq

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.CatchIO as MCIO
import qualified Data.Time as Time
import qualified Snap

data LoggedException = LoggedException
  { leException :: !SomeException
  , leLoggedAt :: !Time.UTCTime
  , leRequest :: !Snap.Request
  } deriving (Show)

data ErrorCollectorConfig = ErrorCollectorConfig
  { ecFlush :: !(Time.UTCTime -> Seq LoggedException -> IO ())
    -- ^ An IO action to perform with the list of exceptions that were
    -- thrown during the last collection period. The computation will be
    -- executed asynchronously, but subsequent collections will not be
    -- flushed until outstanding computations complete.

  , ecFlushInterval :: !Int
    -- ^ How long (in microseconds) to collect exceptions for until they are sent
    -- (via 'ecFlush'). You can pass '0' here, in which case @snap-error-collector@
    -- will idle until an exception happens.

  , ecFilter :: !(SomeException -> Bool)
    -- ^ A filter on which exceptions should be collected. SomeException's that
    -- return true under this predicate will be collected, other errors will be
    -- not.
  }

collectErrors :: ErrorCollectorConfig -> Snap.Initializer b v ()
collectErrors ErrorCollectorConfig{..} =
  do q <- liftIO STM.newTQueueIO
     worker <- liftIO (Async.async (forever (processQueue q)))
     addWrapper q
     Snap.onUnload (Async.cancel worker)
  where addWrapper q =
          Snap.wrapSite
            (\h ->
               do ex <- MCIO.try h
                  case ex of
                    Left se ->
                      do now <- liftIO Time.getCurrentTime
                         req <- Snap.getRequest
                         when (ecFilter se)
                              (liftIO (STM.atomically
                                         (STM.writeTQueue q
                                                          (LoggedException se now req))))
                         MCIO.throw se
                    Right a -> return a)
        processQueue q =
          do threadDelay ecFlushInterval
             exceptions <- STM.atomically
                             (do liftA2 (<|)
                                        (STM.readTQueue q)
                                        (unfoldM' (STM.tryReadTQueue q)))
             when (not (Seq.null exceptions))
                  (do now <- Time.getCurrentTime
                      ecFlush now exceptions)
