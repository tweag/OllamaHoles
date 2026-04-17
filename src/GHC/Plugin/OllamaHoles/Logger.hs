{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Logger
    ( Logger()
    , initLogger
    , writeLogEvent
    , LogEvent()
    , mkLogEvent
    ) where



import qualified Crypto.Hash as H
import           Data.Aeson (encode, FromJSON(..), ToJSON(..), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
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
        { logEvent = recordLogEvent logConfig
        , config   = logConfig
        }

recordLogEvent :: LogConfig -> LogEvent -> IO ()
recordLogEvent config event = do
    now <- getFormattedTimestamp
    let record = renderLogEvent now config event
    putStrLn "XYZZY"

renderLogEvent
    :: Timestamp -> LogConfig -> LogEvent
    -> (LBS.ByteString, PromptHash, ResponseHash)
renderLogEvent now config event =
    let promptHash = contentHash $ lePrompt event
        responseHash = contentHash $ leResponse event
        line = encode $ Aeson.Object $ KM.fromList
            [ (Key.fromText "timestamp"        , toJSON now)
            , (Key.fromText "session_id"       , toJSON $ unSessionId $ sessionId config)
            , (Key.fromText "suggestion_count" , toJSON $ leSuggestionCount event)
            , (Key.fromText "unique_count"     , toJSON $ leUniqueCount event)
            , (Key.fromText "valid_count"      , toJSON $ leValidCount event)
            , (Key.fromText "prompt_hash"      , toJSON promptHash)
            , (Key.fromText "response_hash"    , toJSON responseHash)
            ]
    in (line, promptHash, responseHash)

-- | Invoke a logger (this is exposed)
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

getFormattedTimestamp :: IO Timestamp
getFormattedTimestamp = do
  now <- getCurrentTime
  pure . T.pack $ formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S" now

contentHash :: T.Text -> T.Text
contentHash =
  T.pack . show . (H.hash . TE.encodeUtf8 :: T.Text -> H.Digest H.SHA256)



-- Events

data LogEvent = LogEvent
  { lePrompt          :: Prompt
  , leResponse        :: Response
  , leSuggestionCount :: SuggestionCount -- returned by the LLM
  , leUniqueCount     :: UniqueCount     -- deduplicated (llm may be redundant)
  , leValidCount      :: ValidCount      -- how many unique candidates where valid
  }

type Prompt          = T.Text
type Response        = T.Text
type SuggestionCount = Int
type UniqueCount     = Int
type ValidCount      = Int
type Timestamp       = T.Text
type PromptHash      = T.Text
type ResponseHash    = T.Text

-- | Smart constructor
mkLogEvent :: LogEvent
mkLogEvent = LogEvent mempty mempty 0 0 0



-- Helpers

newtype SessionId = SessionId { unSessionId :: T.Text }
  deriving (Eq, Ord, Show, Generic)

genSessionId :: IO SessionId
genSessionId = do
  w <- randomIO :: IO Word32
  -- Note: showHex is a ShowS, which prepends the (hex
  -- representation of) the left argument onto the right.
  pure $ SessionId $ T.pack $ take 8 $ showHex w "00000000"
