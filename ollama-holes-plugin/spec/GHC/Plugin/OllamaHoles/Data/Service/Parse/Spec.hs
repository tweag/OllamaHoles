module GHC.Plugin.OllamaHoles.Data.Service.Parse.Spec
  ( tests
  ) where

import Data.Either (isLeft)
import Data.Functor ((<&>))
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import Test.Tasty.QuickCheck qualified as QC
import Toml qualified as Toml
import Toml.Schema qualified as Toml

import Toml.TestHelper

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Backend
  (OpenAIConfig(..), GeminiConfig(..), OllamaConfig(..), BackendConfig(..))

import GHC.Plugin.OllamaHoles.Data.Service.Types.Gen



tests :: TestTree
tests = testGroup "Service.Parse"
  [ tests_tomlServiceName_unit
  , tests_tomlServiceName_prop
  , tests_tomlService_unit
  , tests_tomlService_prop
  ]



tests_tomlServiceName_unit :: TestTree
tests_tomlServiceName_unit = testGroup "tomlServiceName (unit)"
  [ testCase "tomlServiceName parses a TOML string value" $
      Toml.runMatcherIgnoreWarn (tomlServiceName (Toml.Text "svc"))
        @?= Right (ServiceName "svc")

  , testCase "tomlServiceName rejects a non-string TOML value" $
      assertBool
        "expected tomlServiceName to reject integer values"
        (isLeft (Toml.runMatcherIgnoreWarn (tomlServiceName (Toml.Integer 7))))
  ]

tests_tomlServiceName_prop :: TestTree
tests_tomlServiceName_prop = testGroup "tomlServiceName (prop)"
  [ QC.testProperty "tomlServiceName parses generated string values" $
      QC.forAll genServiceNameText $ \name ->
        Toml.runMatcherIgnoreWarn (tomlServiceName (Toml.Text name))
          QC.=== Right (ServiceName name)
  ]

tests_tomlService_unit :: TestTree
tests_tomlService_unit = testGroup "tomlService (unit)"
  [ testGroup "success" $
      tests_tomlService_unit_success <&> \(name, txt, expect) ->
        testCase name $ assertTomlParsesAs tomlService txt expect

  , testGroup "failure" $
      tests_tomlService_unit_failure <&> \(name, txt) ->
        testCase name $ assertTomlParseFails tomlService txt
  ]

tests_tomlService_prop :: TestTree
tests_tomlService_prop = testGroup "tomlService (prop)"
  [ QC.testProperty "generated minimal Ollama services parse" $
    QC.forAll genServiceNameText $ \name ->
      propTomlParseSuccess tomlService
        ( "name = '" <> name <> "'\n\
          \protocol = 'ollama'"
        , Service
          { svcName = ServiceName name
          , svcConfig = SvcOllama (OllamaConfig Nothing)
          }
        )

  , QC.testProperty "generated Ollama services with hosts parse" $
    QC.forAll genServiceNameText $ \name ->
    QC.forAll genHostText $ \host ->
      propTomlParseSuccess tomlService
        ( "name = '" <> name <> "'\n\
          \protocol = 'ollama'\n\
          \host = '" <> host <> "'"
        , Service
          { svcName = ServiceName name
          , svcConfig = SvcOllama (OllamaConfig (Just host))
          }
        )

  , QC.testProperty "generated OpenAI services parse" $
    QC.forAll genServiceNameText $ \name ->
    QC.forAll genUrlText $ \baseUrl ->
    QC.forAll genEnvVarText $ \keyName ->
      propTomlParseSuccess tomlService
        ( "name ='" <> name <> "'\n\
          \protocol ='openai'\n\
          \base_url ='" <> baseUrl <> "'\n\
          \key_name ='" <> keyName <> "'"
        , Service
          { svcName = ServiceName name
          , svcConfig = SvcOpenAI (OpenAIConfig baseUrl keyName)
          }
        )

  , QC.testProperty "generated OpenAI-compatible services parse as OpenAI config" $
    QC.forAll genServiceNameText $ \name ->
    QC.forAll genUrlText $ \baseUrl ->
    QC.forAll genEnvVarText $ \keyName ->
      propTomlParseSuccess tomlService
        ( "name ='" <> name <> "'\n\
          \protocol = 'openai-compatible'\n\
          \base_url = '" <> baseUrl <> "'\n\
          \key_name = '" <> keyName <> "'"
        , Service
          { svcName = ServiceName name
          , svcConfig = SvcOpenAI (OpenAIConfig baseUrl keyName)
          }
        )

  , QC.testProperty "generated Gemini services parse" $
    QC.forAll genServiceNameText $ \name ->
    QC.forAll genEnvVarText $ \keyName ->
      propTomlParseSuccess tomlService
        ( "name = '" <> name <> "'\n\
          \protocol = 'gemini'\n\
          \key_name = '" <> keyName <> "'"
        , Service
          { svcName = ServiceName name
          , svcConfig = SvcGemini (GeminiConfig keyName)
          }
        )
  ]



tests_tomlService_unit_success
  :: [(String, Text, Service)]
tests_tomlService_unit_success =
  [ ( "parses minimal Ollama service"
    , "name = 'local'\n\
        \protocol = 'ollama'"
    , Service
        { svcName = ServiceName "local"
        , svcConfig = SvcOllama (OllamaConfig Nothing)
        }
    )

  , ( "parses Ollama service with host"
    , "name = 'local'\n\
        \protocol = 'ollama'\n\
        \host = 'http://localhost:11434'"
    , Service
        { svcName = ServiceName "local"
        , svcConfig = SvcOllama
            (OllamaConfig (Just "http://localhost:11434"))
        }
    )

  , ( "parses OpenAI service"
    , "name = 'openai'\n\
        \protocol = 'openai'\n\
        \base_url = 'https://api.openai.com'\n\
        \key_name = 'OPENAI_API_KEY'"
    , Service
        { svcName = ServiceName "openai"
        , svcConfig = SvcOpenAI
            (OpenAIConfig "https://api.openai.com" "OPENAI_API_KEY")
        }
    )

  , ( "parses OpenAI-compatible service as OpenAI backend config"
    , "name = 'local-openai-compatible'\n\
        \protocol = 'openai-compatible'\n\
        \base_url = 'http://localhost:8080/v1'\n\
        \key_name = 'LOCAL_KEY'"
    , Service
        { svcName = ServiceName "local-openai-compatible"
        , svcConfig = SvcOpenAI
            (OpenAIConfig "http://localhost:8080/v1" "LOCAL_KEY")
        }
    )

  , ( "parses Gemini service"
    , "name = 'gemini'\n\
        \protocol = 'gemini'\n\
        \key_name = 'GEMINI_API_KEY'"
    , Service
        { svcName = ServiceName "gemini"
        , svcConfig = SvcGemini
            (GeminiConfig "GEMINI_API_KEY")
        }
    )
  ]

tests_tomlService_unit_failure
  :: [(String, Text)]
tests_tomlService_unit_failure =
  [ ( "rejects missing name"
    , "protocol = 'ollama'"
    )

  , ( "rejects missing protocol"
    , "name = 'local'"
    )

  , ( "rejects invalid protocol"
    , "name = 'bad'\n\
        \protocol = 'bogus'"
    )

  , ( "rejects OpenAI service missing base_url"
    , "name = 'openai'\n\
        \protocol = 'openai'\n\
        \key_name = 'OPENAI_API_KEY'"
    )

  , ( "rejects OpenAI service missing key_name"
    , "name = 'openai'\n\
        \protocol = 'openai'\n\
        \base_url = 'https://api.openai.com'"
    )

  , ( "rejects Gemini service missing key_name"
    , "name = 'gemini'\n\
        \protocol = 'gemini'"
    )
  ]
