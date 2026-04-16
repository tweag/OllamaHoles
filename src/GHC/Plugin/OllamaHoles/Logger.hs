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
    } deriving (Eq, Show, Generic)

initLogConfig :: IO LogConfig
initLogConfig = do
    sId <- genSessionId
    pure $ LogConfig
        { sessionId = sId
        }



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
