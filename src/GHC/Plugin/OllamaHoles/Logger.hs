{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Logger
    ( Logger()
    , initLogger
    , writeLogEvent
    , LogEvent()
    , mkLogEvent
    , Prompt
    , Response
    ) where



import           Control.Monad (unless)
import qualified Crypto.Hash as H
import           Data.Aeson (encode, FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encoding (pairs, encodingToLazyByteString)
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
import           System.FilePath ((</>), dropTrailingPathSeparator, takeDirectory)
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
    let (record, promptHash, responseHash) = renderLogEvent now config event
    case logMode config of
        LogOff ->
            pure ()

        LogBasic -> do
            appendEventLine config record

        LogFull -> do
            appendEventLine config record
            _ <- writeBlobIfAbsent config BlobPrompt   (BlobHash promptHash)   (lePrompt event)
            _ <- writeBlobIfAbsent config BlobResponse (BlobHash responseHash) (leResponse event)
            pure ()

renderLogEvent
    :: Timestamp -> LogConfig -> LogEvent
    -> (LBS.ByteString, PromptHash, ResponseHash)
renderLogEvent now config event =
    let promptHash = contentHash $ lePrompt event
        responseHash = contentHash $ leResponse event

        enc :: Aeson.Encoding
        enc =
            -- Manually encoding so we have control over key order
            pairs $
                "timestamp"           .= now
                <> "session_id"       .= unSessionId (sessionId config)
                <> "suggestion_count" .= leSuggestionCount event
                <> "unique_count"     .= leUniqueCount event
                <> "valid_count"      .= leValidCount event
                <> "prompt_hash"      .= promptHash
                <> "response_hash"    .= responseHash

        line = encodingToLazyByteString enc
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
    , logMode   :: LogMode
    } deriving (Eq, Show, Generic)

data LogMode
  = LogOff
  | LogBasic -- JSONL events only
  | LogFull  -- JSONL events + prompt/response blobs
  deriving (Eq, Show, Generic)

initLogConfig :: IO LogConfig
initLogConfig = do
    sId <- genSessionId
    paths <- mkDefaultLogPaths Nothing
    pure $ LogConfig
        { sessionId = sId
        , logPaths  = paths
        , logMode   = LogFull
        }

data LogPaths = LogPaths
    { lpRootDir   :: FilePath -- e.g. ~/.local/state/ollama-holes
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
          , lpBlobDir   = root </> "blob"
          }
    ensureLogDirs paths
    pure paths

-- | Create any log directories that don't exist.
ensureLogDirs :: LogPaths -> IO ()
ensureLogDirs paths = do
    createDirectoryIfMissing True $ lpRootDir paths
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
type Blob            = T.Text

-- | Smart constructor
mkLogEvent
    :: Prompt -> Response
    -> SuggestionCount -> UniqueCount -> ValidCount
    -> LogEvent
mkLogEvent = LogEvent



-- Helpers

newtype SessionId = SessionId { unSessionId :: T.Text }
  deriving (Eq, Ord, Show, Generic)

genSessionId :: IO SessionId
genSessionId = do
  w <- randomIO :: IO Word32
  -- Note: showHex is a ShowS, which prepends the (hex
  -- representation of) the left argument onto the right.
  pure $ SessionId $ T.pack $ take 8 $ showHex w "00000000"



-- Blobs

data BlobKind
    = BlobPrompt
    | BlobResponse
    deriving (Eq, Ord, Show, Generic)

data BlobHash = BlobHash
    { unBlobHash :: T.Text
    } deriving (Eq, Ord, Show, Generic)

mkBlobPath :: LogPaths -> BlobKind -> BlobHash -> FilePath
mkBlobPath LogPaths{lpBlobDir} kind (BlobHash h) =
  let bucket = case kind of
          BlobPrompt   -> "prompt"
          BlobResponse -> "response"

      hs = T.unpack h
      shard = case hs of
          a:b:_ -> [a,b]
          _     -> "00"
  in dropTrailingPathSeparator lpBlobDir </> bucket </> shard </> hs

writeBlobIfAbsent
  :: LogConfig -> BlobKind -> BlobHash -> T.Text -> IO FilePath
writeBlobIfAbsent config kind h txt = do
  let fp = mkBlobPath (logPaths config) kind h
  createDirectoryIfMissing True (takeDirectory fp)
  exists <- doesFileExist fp
  unless exists $ BS.writeFile fp (TE.encodeUtf8 txt)
  pure fp

appendEventLine
  :: LogConfig -> LBS.ByteString -> IO ()
appendEventLine config line = do
  let fp = eventsFileForSession (logPaths config)
  createDirectoryIfMissing True (takeDirectory fp)
  LBS.appendFile fp (line <> "\n")

eventsFileForSession :: LogPaths -> FilePath
eventsFileForSession paths =
    let dir = lpRootDir paths
    in dropTrailingPathSeparator dir </> "hole-fit-logs.jsonl"
