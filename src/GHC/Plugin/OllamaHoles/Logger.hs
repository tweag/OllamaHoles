module GHC.Plugin.OllamaHoles.Logger
    ( Logger()
    , initLogger
    , writeLogEvent
    , LogEvent()
    , mkLogEvent
    ) where



-- | Opaque logging object
data Logger = Logger
    { logEvent :: LogEvent -> IO ()
    }

-- | Initialize a logger
initLogger :: IO Logger
initLogger = pure $ Logger
    { logEvent = \_ -> putStrLn "XYZZY"
    }

-- | Invoke a logger
writeLogEvent :: Logger -> LogEvent -> IO ()
writeLogEvent logger event = do
    let Logger write = logger
    write event



data LogEvent = LogEvent

-- | Smart constructor
mkLogEvent :: LogEvent
mkLogEvent = LogEvent
