{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Control.Monad.Logger (runLoggingT)
import Control.Concurrent (forkIO, threadDelay)
import Data.Default (def)
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, flushLogStr)
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import Yesod.Core.Types (loggerSet, Logger (Logger))
import Data.IORef
import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import Network.HTTP.Conduit (newManager)

-- adapt to http-conduit 1.x or 2.x when cabal macros are available, otherwise assume 2.x
#ifdef MIN_VERSION_http_conduit
#if MIN_VERSION_http_conduit(2,0,0)
#define http_conduit_2
#endif
#else
#define http_conduit_2
#endif
#ifdef http_conduit_2
import Network.HTTP.Client (defaultManagerSettings)
#else
import Network.HTTP.Conduit (def)
#endif

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.RootR
import Handler.JournalR
import Handler.RegisterR
import Handler.SidebarR

import Hledger.Web.Options (WebOpts(..), defwebopts)
import Hledger.Data (Journal, nulljournal)
import Hledger.Read (readJournalFile)
import Hledger.Utils (error')
import Hledger.Cli.Options (defcliopts, journalFilePathFromOpts)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: WebOpts -> Journal -> AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication _opts j conf = do
    foundation <- makeFoundation conf
    writeIORef (appJournal foundation) j

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
#ifdef http_conduit_2
               defaultManagerSettings
#else
               def
#endif
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)
    jref <- newIORef nulljournal
    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, updater) <- clockDateCacher
    -- If the Yesod logger (as opposed to the request logger middleware) is
    -- used less than once a second on average, you may prefer to omit this
    -- thread and use "(updater >> getter)" in place of "getter" below.  That
    -- would update the cache every time it is used, instead of every second.
    let updateLoop = do
            threadDelay 1000000
            updater
            flushLogStr loggerSet'
            updateLoop
    _ <- forkIO updateLoop

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s p manager dbconf logger defwebopts jref

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
-- uses the journal specified by the LEDGER_FILE env var, or ~/.hledger.journal
getApplicationDev :: IO (Int, Application)
getApplicationDev = do
  f <- journalFilePathFromOpts defcliopts
  j <- either error' id `fmap` readJournalFile Nothing Nothing True f
  -- defaultDevelApp loader (makeApplication defwebopts j)
  defaultDevelApp loader (fmap fst . makeApplication defwebopts j)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
