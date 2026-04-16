{-# LANGUAGE DeriveGeneric #-}
module GHC.Plugin.OllamaHoles.Logger
    ( Logger()
    , initLogger
    , writeLogEvent
    , LogEvent()
    , mkLogEvent
    ) where



import qualified Data.Text as T
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import           Numeric (showHex)
import           System.Directory
    ( XdgDirectory(XdgState)
    , createDirectoryIfMissing
    , doesFileExist
    , getXdgDirectory
    )
import           System.FilePath ((</>))
import           System.Random (randomIO)



-- | Opaque logging object
data Logger = Logger
    { logEvent :: LogEvent -> IO ()
    , config   :: LogConfig
    }

-- | Initialize a logger
initLogger :: IO Logger
initLogger = do
    logConfig <- initLogConfig
    pure $ Logger
      { logEvent = \_ -> putStrLn "XYZZY"
      , config   = logConfig
      }

-- | Invoke a logger
writeLogEvent :: Logger -> LogEvent -> IO ()
writeLogEvent logger event = do
    let Logger write config = logger
    write event



-- Configuration

data LogConfig = LogConfig
    { sessionId :: SessionId
    , logPaths  :: LogPaths
    } deriving (Eq, Show, Generic)

initLogConfig :: IO LogConfig
initLogConfig = do
    sId <- genSessionId
    paths <- mkDefaultLogPaths Nothing
    pure $ LogConfig
        { sessionId = sId
        }

data LogPaths = LogPaths
    { lpRootDir   :: FilePath -- e.g. ~/.local/state/ollama-holes
    , lpEventsDir :: FilePath -- e.g. ~/.local/state/ollama-holes/events
    , lpBlobDir   :: FilePath -- e.g. ~/.local/state/ollama-holes/blob
    } deriving (Eq, Show, Generic)

-- | Get the default location for storing logs
mkDefaultLogPaths
    :: Maybe FilePath -- Optional root directory
    -> IO LogPaths
mkDefaultLogPaths mRoot = do
    root <- case mRoot of
        Just fp -> pure fp
        Nothing -> getXdgDirectory XdgState "ollama-holes"
    let paths = LogPaths
          { lpRootDir   = root
          , lpEventsDir = root </> "events"
          , lpBlobDir   = root </> "blob"
          }
    ensureLogDirs paths
    pure paths

-- | Create any log directories that don't exist.
ensureLogDirs :: LogPaths -> IO ()
ensureLogDirs paths = do
    createDirectoryIfMissing True $ lpRootDir paths
    createDirectoryIfMissing True $ lpEventsDir paths
    createDirectoryIfMissing True $ lpBlobDir paths



-- Events

data LogEvent = LogEvent

-- | Smart constructor
mkLogEvent :: LogEvent
mkLogEvent = LogEvent



-- Helpers

newtype SessionId = SessionId T.Text
  deriving (Eq, Ord, Show, Generic)

genSessionId :: IO SessionId
genSessionId = do
  w <- randomIO :: IO Word32
  -- Note: showHex is a ShowS, which prepends the (hex
  -- representation of) the left argument onto the right.
  pure $ SessionId $ T.pack $ take 8 $ showHex w "00000000"
