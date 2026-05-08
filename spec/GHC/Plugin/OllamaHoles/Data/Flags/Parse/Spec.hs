module GHC.Plugin.OllamaHoles.Data.Flags.Parse.Spec (tests) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import GHC.Driver.Plugins (CommandLineOption(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Logger (LogMode(..))
import GHC.Plugin.OllamaHoles.Data.Trigger (TriggerPolicy(..))
import GHC.Plugin.OllamaHoles.Data.Flags
  (Flags(..), FlagToken(..), ConfigPathSpec(..), parseFlags)
import GHC.Plugin.OllamaHoles.Template (unsafeCreateRawTemplateName)



tests :: TestTree
tests = testGroup "Flags.Parse"
  [ test_parseFlags_unit
  , test_parseFlags_prop
  ]

test_parseFlags_unit :: TestTree
test_parseFlags_unit = testGroup "parseFlags (unit)"
  [ testGroup "success" $
      tests_parseFlags_unit_success <&> \(name, input, expected) ->
        testCase name $ case parseFlags input of
          Left err -> assertFailure $
            "expected successful parse but got this error: " <> show err
          Right actual -> actual @?= expected

  , testGroup "failure" $
      tests_parseFlags_unit_failure <&> \(name, input) ->
        testCase name $ case parseFlags input of
          Left _ -> pure ()
          Right ok -> assertFailure $
            "expected failed parse but got this result: " <> show ok
  ]

test_parseFlags_prop :: TestTree
test_parseFlags_prop = testGroup "parseFlags (prop)"
  []



tests_parseFlags_unit_success
  :: [(String, [CommandLineOption], (Flags, [FlagToken]))]
tests_parseFlags_unit_success =
  [ ( "empty options yields default flags and no unknowns"
    , []
    , ( mempty
      , []
      )
    )

  -- Simple Options
  -----------------

  , ( "model= sets model_name"
    , ["model=phi4"]
    , ( mempty { model_name = Just "phi4" }
      , []
      )
    )

  , ( "backend=ollama sets backend_name"
    , ["backend=ollama"]
    , ( mempty { backend_name = Just Ollama }
      , []
      )
    )

  , ( "backend=openai sets backend_name"
    , ["backend=openai"]
    , ( mempty { backend_name = Just OpenAI }
      , []
      )
    )

  , ( "backend=gemini sets backend_name"
    , ["backend=gemini"]
    , ( mempty { backend_name = Just Gemini }
      , []
      )
    )

  , ( "openai_base_url= sets openai_base_url"
    , ["openai_base_url=https://example.com/v1"]
    , ( mempty { openai_base_url = Just "https://example.com/v1" }
      , []
      )
    )

  , ( "openai_key_name= sets openai_key_name"
    , ["openai_key_name=MY_API_KEY"]
    , ( mempty { openai_key_name = Just "MY_API_KEY" }
      , []
      )
    )

  , ( "debug enables debug"
    , ["debug"]
    , ( mempty { debug = Just True }
      , []
      )
    )

  , ( "nodebug disables debug"
    , ["nodebug"]
    , ( mempty { debug = Just False }
      , []
      )
    )

  , ( "include-docs enables include_docs"
    , ["include-docs"]
    , ( mempty { include_docs = Just True }
      , []
      )
    )

  , ( "no-include-docs disables include_docs"
    , ["no-include-docs"]
    , ( mempty { include_docs = Just False }
      , []
      )
    )

  , ( "n= sets num_expr"
    , ["n=17"]
    , ( mempty { num_expr = Just 17 }
      , []
      )
    )

  , ( "log=off sets log_mode"
    , ["log=off"]
    , ( mempty { log_mode = Just LogOff }
      , []
      )
    )

  , ( "log=basic sets log_mode"
    , ["log=basic"]
    , ( mempty { log_mode = Just LogBasic }
      , []
      )
    )

  , ( "log=full sets log_mode"
    , ["log=full"]
    , ( mempty { log_mode = Just LogFull }
      , []
      )
    )

  , ( "log-dir= sets log_dir"
    , ["log-dir=/tmp/ollama-holes-logs"]
    , ( mempty { log_dir = Just "/tmp/ollama-holes-logs" }
      , []
      )
    )

  , ( "model-options= parses valid JSON object"
    , ["model-options={\"temperature\":1.0,\"num_ctx\":32000}"]
    , ( mempty { model_options = Just $ Aeson.object
          [ "temperature" .= (1.0 :: Double)
          , "num_ctx" .= (32000 :: Int)
          ] }
      , []
      )
    )

  , ( "template= sets template_path and clears template_name"
    , ["template=/tmp/prompt.txt"]
    , ( mempty
        { template_path = Just "/tmp/prompt.txt"
        , template_name = Nothing
        }
      , []
      )
    )

  , ( "template-name= sets template_name and clears template_path"
    , ["template-name=qwen"]
    , ( mempty
        { template_path = Nothing
        , template_name = Just (unsafeCreateRawTemplateName "qwen")
        }
      , []
      )
    )

  , ( "template-dir= sets template_search_dir"
    , ["template-dir=/tmp/templates"]
    , ( mempty { template_search_dir = Just "/tmp/templates" }
      , []
      )
    )

  , ( "trigger=all sets trigger_policy"
    , ["trigger=all"]
    , ( mempty { trigger_policy = Just TriggerAll }
      , []
      )
    )

  , ( "trigger=none sets trigger_policy"
    , ["trigger=none"]
    , ( mempty { trigger_policy = Just TriggerNone }
      , []
      )
    )

  , ( "trigger=prefix:llm sets trigger_policy"
    , ["trigger=prefix:llm"]
    , ( mempty { trigger_policy = Just (TriggerPrefix "llm") }
      , []
      )
    )

  , ( "config=none disables config file loading"
    , ["config=none"]
    , ( mempty { config_path = Just ConfigDisabled }
      , []
      )
    )

  , ( "config=default requests default config file"
    , ["config=default"]
    , ( mempty { config_path = Just ConfigDefault }
      , []
      )
    )

  , ( "config=path sets explicit config path"
    , ["config=/tmp/ollama-holes.toml"]
    , ( mempty { config_path = Just (ConfigExplicit "/tmp/ollama-holes.toml") }
      , []
      )
    )

  -- Unknown Options
  ------------------

  , ( "unknown boolean options are reported and do not change flags"
    , ["bogus-option"]
    , ( mempty
      , [ BooleanToken "bogus-option"
        ]
      )
    )

  , ( "unknown value options are reported and do not change flags"
    , ["another=thing"]
    , ( mempty
      , [ ValueToken "another" "thing"
        ]
      )
    )

  , ( "unknown options are reported and do not change flags"
    , ["bogus-option", "another=thing"]
    , ( mempty
      , [ BooleanToken "bogus-option"
        , ValueToken "another" "thing"
        ]
      )
    )

  , ( "unknown options are reported but do not block recognized options"
    , ["bogus-option", "model=qwen3", "debug"]
    , ( mempty
        { model_name = Just "qwen3"
        , debug = Just True
        }
      , [ BooleanToken "bogus-option"
        ]
      )
    )

  , ( "value-like unknown options are reported but do not block recognized options"
    , ["bogus=thing", "backend=openai"]
    , ( mempty
        { backend_name = Just OpenAI
        }
      , [ ValueToken "bogus" "thing"
        ]
      )
    )

  -- Precedence
  -------------

  , ( "leftmost model= wins"
    , ["model=first", "model=second"]
    , ( mempty { model_name = Just "first" }
      , []
      )
    )

  , ( "leftmost backend= wins"
    , ["backend=ollama", "backend=openai"]
    , ( mempty { backend_name = Just Ollama }
      , []
      )
    )

  , ( "leftmost openai_base_url= wins"
    , [ "openai_base_url=https://first.example"
      , "openai_base_url=https://second.example"
      ]
    , ( mempty { openai_base_url = Just "https://first.example" }
      , []
      )
    )

  , ( "leftmost openai_key_name= wins"
    , [ "openai_key_name=FIRST_KEY"
      , "openai_key_name=SECOND_KEY"
      ]
    , ( mempty { openai_key_name = Just "FIRST_KEY" }
      , []
      )
    )

  , ( "leftmost n= wins"
    , ["n=3", "n=9"]
    , ( mempty { num_expr = Just 3 }
      , []
      )
    )

  , ( "leftmost model-options= wins"
    , [ "model-options={\"temperature\":0.1}"
      , "model-options={\"temperature\":0.9}"
      ]
    , ( mempty { model_options = Just $ Aeson.object
          [ "temperature" .= (0.1 :: Double)
          ] }
      , []
      )
    )

  , ( "leftmost log= wins"
    , ["log=basic", "log=off"]
    , ( mempty { log_mode = Just LogBasic }
      , []
      )
    )

  , ( "leftmost log-dir= wins"
    , [ "log-dir=/tmp/one"
      , "log-dir=/tmp/two"
      ]
    , ( mempty { log_dir = Just "/tmp/one" }
      , []
      )
    )

  , ( "leftmost template selector wins: path then name keeps path"
    , [ "template=/tmp/a.txt"
      , "template-name=qwen"
      ]
    , ( mempty
        { template_path = Just "/tmp/a.txt"
        , template_name = Nothing
        }
      , []
      )
    )

  , ( "leftmost template selector wins: name then path keeps name"
    , [ "template-name=qwen"
      , "template=/tmp/a.txt"
      ]
    , ( mempty
        { template_path = Nothing
        , template_name = Just (unsafeCreateRawTemplateName "qwen")
        }
      , []
      )
    )

  , ( "leftmost template-dir= wins"
    , [ "template-dir=/tmp/one"
      , "template-dir=/tmp/two"
      ]
    , ( mempty { template_search_dir = Just "/tmp/one" }
      , []
      )
    )

  , ( "leftmost trigger= wins"
    , [ "trigger=prefix:llm"
      , "trigger=all"
      ]
    , ( mempty { trigger_policy = Just (TriggerPrefix "llm") }
      , []
      )
    )

  , ( "leftmost config= wins"
    , [ "config=none"
      , "config=/tmp/ollama-holes.toml"
      ]
    , ( mempty { config_path = Just ConfigDisabled }
      , []
      )
    )

  , ( "boolean flags are sticky"
    , ["debug", "debug", "include-docs", "include-docs"]
    , ( mempty
        { debug = Just True
        , include_docs = Just True
        }
      , []
      )
    )

  , ( "leftmost wins even when interleaved with other options"
    , [ "model=first"
      , "debug"
      , "backend=ollama"
      , "model=second"
      , "include-docs"
      , "backend=openai"
      ]
    , ( mempty
        { model_name = Just "first"
        , backend_name = Just Ollama
        , debug = Just True
        , include_docs = Just True
        }
      , []
      )
    )

  , ( "log options interleave with other options"
    , [ "model=qwen3"
      , "log=basic"
      , "debug"
      , "log-dir=/tmp/logs"
      ]
    , ( mempty
        { model_name = Just "qwen3"
        , debug = Just True
        , log_mode = Just LogBasic
        , log_dir = Just "/tmp/logs"
        }
      , []
      )
    )

  , ( "trigger interleaves with other options"
    , [ "model=qwen3"
      , "trigger=prefix:foo"
      , "debug"
      ]
    , ( mempty
        { model_name = Just "qwen3"
        , debug = Just True
        , trigger_policy = Just (TriggerPrefix "foo")
        }
      , []
      )
    )

  , ( "leftmost template selector still wins when interleaved with dir flags"
    , [ "template-name=alpha"
      , "template-dir=/tmp/one"
      , "template=/tmp/prompt.txt"
      , "template-dir=/tmp/two"
      ]
    , ( mempty
        { template_name = Just (unsafeCreateRawTemplateName "alpha")
        , template_path = Nothing
        , template_search_dir = Just "/tmp/one"
        }
      , []
      )
    )

  -- Edge Cases
  -------------

  , ( "template-dir combines with template-name"
    , [ "template-dir=/tmp/templates"
      , "template-name=qwen"
      ]
    , ( mempty
        { template_search_dir = Just "/tmp/templates"
        , template_name = Just (unsafeCreateRawTemplateName "qwen")
        , template_path = Nothing
        }
      , []
      )
    )

  , ( "template-dir combines with template path"
    , [ "template-dir=/tmp/templates"
      , "template=/tmp/prompt.txt"
      ]
    , ( mempty
        { template_search_dir = Just "/tmp/templates"
        , template_path = Just "/tmp/prompt.txt"
        , template_name = Nothing
        }
      , []
      )
    )
  ]

tests_parseFlags_unit_failure
  :: [(String, [CommandLineOption])]
tests_parseFlags_unit_failure =
  [ ( "empty option is an error", [""] )

  -- Absent Values
  ----------------

  , ( "missing value for model is an error",           ["model"] )
  , ( "missing value for backend is an error",         ["backend"] )
  , ( "missing value for openai_base_url is an error", ["openai_base_url"] )
  , ( "missing value for openai_key_name is an error", ["openai_key_name"] )
  , ( "missing value for n is an error",               ["n"] )
  , ( "missing value for model-options is an error",   ["model-options"] )
  , ( "missing value for log is an error",             ["log"] )
  , ( "missing value for log-dir is an error",         ["log-dir"] )
  , ( "missing value for template is an error",        ["template"] )
  , ( "missing value for template-name is an error",   ["template-name"] )
  , ( "missing value for template-dir is an error",    ["template-dir"] )
  , ( "missing value for trigger is an error",         ["trigger"] )
  , ( "missing value for config is an error",          ["config"] )

  -- Empty Values
  ---------------

  , ( "empty model value is an error",           ["model="] )
  , ( "empty backend value is an error",         ["backend="] )
  , ( "empty openai_base_url value is an error", ["openai_base_url="] )
  , ( "empty openai_key_name value is an error", ["openai_key_name="] )
  , ( "empty n value is an error",               ["n="] )
  , ( "empty model-options value is an error",   ["model-options="] )
  , ( "empty log value is an error",             ["log="] )
  , ( "empty log-dir value is an error",         ["log-dir="] )
  , ( "empty template value is an error",        ["template="] )
  , ( "empty template-name value is an error",   ["template-name="] )
  , ( "empty template-dir value is an error",    ["template-dir="] )
  , ( "empty trigger value is an error",         ["trigger="] )
  , ( "empty config value is an error",          ["config="] )

  -- Unexpected Values
  --------------------

  , ( "unexpected value for debug is an error",           ["debug=true"] )
  , ( "unexpected value for nodebug is an error",         ["nodebug=true"] )
  , ( "unexpected value for include-docs is an error",    ["include-docs=yes"] )
  , ( "unexpected value for no-include-docs is an error", ["no-include-docs=yes"] )

  -- Invalid Values
  -----------------

  , ( "invalid n returns an error",             ["n=not-an-int"] )
  , ( "invalid backend is a an error",          ["backend=weird"] )
  , ( "invalid log mode is a an error",         ["log=weird"] )
  , ( "invalid model-options returns an error", ["model-options={not json}"] )
  , ( "invalid template name is rejected",      ["template-name=../secrets"] )
  , ( "invalid trigger policy is rejected",     ["trigger=prefix:"] )
  , ( "invalid trigger prefix is rejected",     ["trigger=prefix:abc-"] )
  ]
