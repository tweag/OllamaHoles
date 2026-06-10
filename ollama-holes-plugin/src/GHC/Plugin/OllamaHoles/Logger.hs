module GHC.Plugin.OllamaHoles.Logger
    ( Logger()
    , LogMode(..)
    , initLogger
    , writeLogEvent
    , LogEvent()
    , mkLogEvent
    , Prompt
    , Response
    ) where



import           Control.Monad (unless)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encoding (pairs, encodingToLazyByteString)
import           Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Word (Word8, Word32, Word64)
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
initLogger :: Maybe LogMode -> Maybe FilePath -> IO Logger
initLogger mMode mRoot = do
    logConfig <- initLogConfig mMode mRoot
    pure Logger
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
    let promptHash = contentHashText $ lePrompt event
        responseHash = contentHashText $ leResponse event

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
    let Logger write _ = logger
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

initLogConfig :: Maybe LogMode -> Maybe FilePath -> IO LogConfig
initLogConfig mMode mRoot = do
    sId <- genSessionId
    let mode = maybe LogFull id mMode
    paths <- mkDefaultLogPaths mRoot
    case mode of
        LogOff   ->
            pure ()
        LogBasic ->
            ensureRootDir paths
        LogFull  -> do
            ensureRootDir paths
            ensureBlobDir paths
    pure LogConfig
        { sessionId = sId
        , logPaths  = paths
        , logMode   = mode
        }

data LogPaths = LogPaths
    { lpRootDir   :: FilePath -- e.g. ~/.local/state/ollama-holes
    , lpBlobDir   :: FilePath -- e.g. ~/.local/state/ollama-holes/blob
    } deriving (Eq, Show, Generic)

-- | Get the default location for storing logs
mkDefaultLogPaths :: Maybe FilePath -> IO LogPaths
mkDefaultLogPaths mRoot = do
    root <- case mRoot of
        Just fp -> pure fp
        Nothing -> getXdgDirectory XdgState "ollama-holes"
    pure LogPaths
        { lpRootDir = root
        , lpBlobDir = root </> "blob"
        }

ensureRootDir :: LogPaths -> IO ()
ensureRootDir paths =
  createDirectoryIfMissing True (lpRootDir paths)

ensureBlobDir :: LogPaths -> IO ()
ensureBlobDir paths =
  createDirectoryIfMissing True (lpBlobDir paths)

getFormattedTimestamp :: IO Timestamp
getFormattedTimestamp = do
  now <- getCurrentTime
  pure . T.pack $ formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S" now



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



-- Utils

contentHashText :: Text -> Text
contentHashText = T.pack . hex64 . fnv1a64 . TE.encodeUtf8

fnv1a64 :: BS.ByteString -> Word64
fnv1a64 = BS.foldl' step 0xcbf29ce484222325
  where
    step :: Word64 -> Word8 -> Word64
    step h b = (h `xor` fromIntegral b) * 0x100000001b3

hex64 :: Word64 -> String
hex64 w =
  let s = showHex w ""
  in replicate (16 - length s) '0' <> s
