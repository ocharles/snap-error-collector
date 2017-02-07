{-# LANGUAGE RecordWildCards #-}
{-|

@snap-error-collector@ extends a 'Snap' application with the ability to monitor
requests for uncaught exceptions. All routes are wrapped with an exception
handler, and exceptions are queued (and optionally filtered). Periodically,
the exception queue is flushed via an 'IO' computation - you can use this
to send emails, notify yourself on Twitter, increment counters, etc.

Example:

@
import "Snap.ErrorCollector"

initApp :: 'Snap.Initializer' MyApp MyApp
initApp = do
  ...
  'collectErrors' 'ErrorCollectorConfig'
    { 'ecFlush' = emailOpsTeam
    , 'ecFlushInterval' = 60000000
    , 'ecFilter' = 'const' 'True'
    , 'ecUpperBound' = 1000
    }

emailOpsTeam :: 'Time.UTCTime' -> 'Seq' 'LoggedException' -> 'Int' -> 'IO' '()'
emailOpsTeam = ...

@

-}
module Snap.ErrorCollector
  ( collectErrors
  , LoggedException(..)
  , ErrorCollectorConfig(..)
  , basicConfig
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (forever, mplus, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (unfoldM')
import Data.Foldable (msum)
import Data.Sequence as Seq

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception.Lifted as MCIO
import qualified Data.Time as Time
import qualified Snap

-- | An exception logged by @snap-error-collector@, tagged with the request that
-- caused the exception, and the time the exception occured.
data LoggedException = LoggedException
  { leException :: !SomeException
  , leLoggedAt :: !Time.UTCTime
  , leRequest :: !Snap.Request
  } deriving (Show)

-- | How @snap-error-collector@ should run.
data ErrorCollectorConfig = ErrorCollectorConfig
  { ecFlush :: !(Time.UTCTime -> Seq LoggedException -> Int -> IO ())
    -- ^ An IO action to perform with the list of exceptions that were
    -- thrown during the last collection period, and the amount of exceptions that
    -- had to be dropped. The computation will be executed asynchronously, but
    -- subsequent collections will not be flushed until outstanding computations
    -- complete.

  , ecFlushInterval :: !Int
    -- ^ How long (in microseconds) to collect exceptions for until they are sent
    -- (via 'ecFlush'). You can pass '0' here, in which case @snap-error-collector@
    -- will idle until an exception happens.

  , ecFilter :: !(SomeException -> Bool)
    -- ^ A filter on which exceptions should be collected. SomeException's that
    -- return true under this predicate will be collected, other errors will be
    -- not.

  , ecExceptionUpperBound :: !Int
    -- ^ The maximum amount of exceptions to store within 'ecFlushInterval'.
    -- Currently, if more exceptions than this are thrown, subsequent exceptions
    -- will be dropped on the floor (and a counter incremented). This allows you
    -- to maintain predictable memory usage if something in the rest of your
    -- application goes horribly wrong.
    --
    -- If dropping exceptions on the floor doesn't suit your needs, please
    -- open a bug report on the issue tracker and we can discuss alternatives.
  }

-- | A convenient constructor for 'ErrorCollectorConfig' that collects up to
-- 100 exceptions and flushes the queue every minute. You have to supply the
-- 'IO' action to run when the queue is flushed.
basicConfig :: (Time.UTCTime -> Seq LoggedException -> Int -> IO ()) -> ErrorCollectorConfig
basicConfig m = ErrorCollectorConfig m 60000000 (const True) 100

-- | Wrap a 'Snap' website to collect errors.
collectErrors :: ErrorCollectorConfig -> Snap.Initializer b v ()
collectErrors ErrorCollectorConfig{..} =
  do q <- liftIO (STM.newTBQueueIO ecExceptionUpperBound)
     dropped <- liftIO (STM.newTVarIO 0)
     worker <- liftIO (Async.async (forever (processQueue q dropped)))
     addWrapper q dropped
     Snap.onUnload (Async.cancel worker)
  where addWrapper q dropped =
          Snap.wrapSite
            (\h ->
               do ex <- MCIO.try h
                  case ex of
                    Left se ->
                      do now <- liftIO Time.getCurrentTime
                         req <- Snap.getRequest
                         when (ecFilter se)
                              (liftIO (STM.atomically
                                         (msum [STM.writeTBQueue q
                                                                 (LoggedException se now req)
                                               ,STM.modifyTVar dropped succ])))
                         MCIO.throw se
                    Right a -> return a)
        processQueue q dropped =
          do threadDelay ecFlushInterval
             (exceptions,dropped) <- STM.atomically
                                       (do exceptions <- liftA2 (<|)
                                                                (STM.readTBQueue q)
                                                                (unfoldM' (STM.tryReadTBQueue q))
                                           dropped <- STM.readTVar dropped <*
                                                      STM.writeTVar dropped 0
                                           return (exceptions,dropped))
             when (not (Seq.null exceptions) ||
                   dropped > 0)
                  (do now <- Time.getCurrentTime
                      ecFlush now exceptions dropped)
