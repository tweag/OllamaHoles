{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Trigger.Spec (tests) where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck ((===))
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Trigger
  ( TriggerPolicy(..)
  , TriggerPolicyError(..)
  , TriggerMatch(..)
  , defaultTriggerPolicy
  , parseTriggerPolicy
  , renderTriggerPolicy
  , shouldTriggerHole
  , matchTriggerPolicy
  , mkTriggeredHoleName
  )

tests :: TestTree
tests =
  testGroup "Trigger"
    [ unitTests
    , propertyTests
    ]


-- Unit tests
-------------

unitTests :: TestTree
unitTests =
  testGroup "unit"
    [ parsingTests
    , matchingTests
    , namingTests
    , defaultTests
    ]

parsingTests :: TestTree
parsingTests =
  testGroup "parseTriggerPolicy"
    [ testCase "parses all" $
        parseTriggerPolicy "all" @?= Right TriggerAll

    , testCase "parses none" $
        parseTriggerPolicy "none" @?= Right TriggerNone

    , testCase "parses prefix policy" $
        parseTriggerPolicy "prefix:foo" @?= Right (TriggerPrefix "foo")

    , testCase "trims outer whitespace" $
        parseTriggerPolicy "   prefix:foo   " @?= Right (TriggerPrefix "foo")

    , testCase "rejects empty policy" $
        parseTriggerPolicy "" @?= Left EmptyTriggerPolicy

    , testCase "rejects unknown policy" $
        parseTriggerPolicy "wat" @?= Left (UnknownTriggerPolicy "wat")

    , testCase "rejects missing prefix after prefix:" $
        parseTriggerPolicy "prefix:" @?= Left MissingTriggerPrefix

    , testCase "rejects leading underscore in prefix" $
        parseTriggerPolicy "prefix:_foo" @?= Left (InvalidTriggerPrefix "_foo")

    , testCase "rejects uppercase-starting prefix" $
        parseTriggerPolicy "prefix:Foo" @?= Left (InvalidTriggerPrefix "Foo")

    , testCase "rejects digit-starting prefix" $
        parseTriggerPolicy "prefix:1foo" @?= Left (InvalidTriggerPrefix "1foo")

    , testCase "rejects hyphen in prefix" $
        parseTriggerPolicy "prefix:foo-bar" @?= Left (InvalidTriggerPrefix "foo-bar")

    , testCase "rejects slash in prefix" $
        parseTriggerPolicy "prefix:foo/bar" @?= Left (InvalidTriggerPrefix "foo/bar")

    , testCase "rejects dot in prefix" $
        parseTriggerPolicy "prefix:foo.bar" @?= Left (InvalidTriggerPrefix "foo.bar")

    , testCase "accepts digits after first character" $
        parseTriggerPolicy "prefix:foo1" @?= Right (TriggerPrefix "foo1")

    , testCase "accepts underscore after first character" $
        parseTriggerPolicy "prefix:foo_bar" @?= Right (TriggerPrefix "foo_bar")

    , testCase "accepts apostrophe after first character" $
        parseTriggerPolicy "prefix:foo'" @?= Right (TriggerPrefix "foo'")
    ]

matchingTests :: TestTree
matchingTests =
  testGroup "matching"
    [ testCase "TriggerAll always matches" $
        matchTriggerPolicy TriggerAll "_anything"
          @?= Just (TriggerMatch "_anything" "_anything")

    , testCase "TriggerNone never matches" $
        matchTriggerPolicy TriggerNone "_anything"
          @?= Nothing

    , testCase "prefix match succeeds on exact prefix hole" $
        matchTriggerPolicy (TriggerPrefix "foo") "_foo"
          @?= Just (TriggerMatch "_foo" "")

    , testCase "prefix match succeeds with numeric suffix" $
        matchTriggerPolicy (TriggerPrefix "foo") "_foo1"
          @?= Just (TriggerMatch "_foo1" "1")

    , testCase "prefix match succeeds with alphabetic suffix" $
        matchTriggerPolicy (TriggerPrefix "foo") "_foodefault"
          @?= Just (TriggerMatch "_foodefault" "default")

    , testCase "prefix match succeeds with underscore in suffix" $
        matchTriggerPolicy (TriggerPrefix "foo") "_foo_bar"
          @?= Just (TriggerMatch "_foo_bar" "_bar")

    , testCase "prefix match succeeds with apostrophe in suffix" $
        matchTriggerPolicy (TriggerPrefix "foo") "_foo'"
          @?= Just (TriggerMatch "_foo'" "'")

    , testCase "prefix match rejects missing leading underscore" $
        matchTriggerPolicy (TriggerPrefix "foo") "foo"
          @?= Nothing

    , testCase "prefix match rejects wrong prefix" $
        matchTriggerPolicy (TriggerPrefix "foo") "_bar"
          @?= Nothing

    , testCase "prefix match rejects invalid suffix characters" $
        matchTriggerPolicy (TriggerPrefix "foo") "_foo-bar"
          @?= Nothing

    , testCase "prefix match rejects empty prefix policy at match time" $
        matchTriggerPolicy (TriggerPrefix "") "_anything"
          @?= Nothing

    , testCase "shouldTriggerHole agrees with matchTriggerPolicy on hit" $
        shouldTriggerHole (TriggerPrefix "foo") "_foo123" @?= True

    , testCase "shouldTriggerHole agrees with matchTriggerPolicy on miss" $
        shouldTriggerHole (TriggerPrefix "foo") "_bar123" @?= False
    ]

namingTests :: TestTree
namingTests =
  testGroup "naming"
    [ testCase "mkTriggeredHoleName reconstructs exact prefix hole" $
        mkTriggeredHoleName "foo" "" @?= "_foo"

    , testCase "mkTriggeredHoleName reconstructs suffixed hole" $
        mkTriggeredHoleName "foo" "1" @?= "_foo1"

    , testCase "empty suffix and default suffix remain distinct" $
        mkTriggeredHoleName "foo" ""
          /= mkTriggeredHoleName "foo" "default"
          @? "expected distinct reconstructed names"

    , testCase "empty suffix and numeric suffix remain distinct" $
        mkTriggeredHoleName "foo" ""
          /= mkTriggeredHoleName "foo" "1"
          @? "expected distinct reconstructed names"
    ]

defaultTests :: TestTree
defaultTests =
  testGroup "defaults"
    [ testCase "default trigger policy is llm prefix" $
        defaultTriggerPolicy @?= TriggerPrefix "llm"

    , testCase "default trigger policy matches _llm" $
        shouldTriggerHole defaultTriggerPolicy "_llm" @?= True

    , testCase "default trigger policy matches _llm1" $
        shouldTriggerHole defaultTriggerPolicy "_llm1" @?= True

    , testCase "default trigger policy rejects plain typed holes" $
        shouldTriggerHole defaultTriggerPolicy "_" @?= False
    ]


-- Property tests
-----------------

propertyTests :: TestTree
propertyTests =
  testGroup "properties"
    [ QC.testProperty "render/parse roundtrip for valid policies" $
        QC.forAll genTriggerPolicy $ \pol ->
          parseTriggerPolicy (renderTriggerPolicy pol) === Right pol

    , QC.testProperty "shouldTriggerHole agrees with matchTriggerPolicy" $
        QC.forAll genTriggerPolicy $ \pol ->
        QC.forAll genHoleName $ \nm ->
          shouldTriggerHole pol nm === isJust (matchTriggerPolicy pol nm)

    , QC.testProperty "prefix matching is bijective with mkTriggeredHoleName" $
        QC.forAll genValidPrefix $ \pfx ->
        QC.forAll genValidSuffix $ \sfx ->
          matchTriggerPolicy (TriggerPrefix pfx) (mkTriggeredHoleName pfx sfx)
            === Just (TriggerMatch (mkTriggeredHoleName pfx sfx) sfx)

    , QC.testProperty "mkTriggeredHoleName distinguishes distinct suffixes" $
        QC.forAll genValidPrefix $ \pfx ->
        QC.forAll genDistinctSuffixPair $ \(s1, s2) ->
          mkTriggeredHoleName pfx s1 /= mkTriggeredHoleName pfx s2

    , QC.testProperty "TriggerNone never matches any generated hole name" $
        QC.forAll genHoleName $ \nm ->
          isNothing (matchTriggerPolicy TriggerNone nm)

    , QC.testProperty "TriggerAll always matches any generated hole name" $
        QC.forAll genHoleName $ \nm ->
          matchTriggerPolicy TriggerAll nm === Just (TriggerMatch nm nm)

    , QC.testProperty "valid prefix policies parse successfully" $
        QC.forAll genValidPrefix $ \pfx ->
          parseTriggerPolicy ("prefix:" <> pfx) === Right (TriggerPrefix pfx)

    , QC.testProperty "constructed triggered names always trigger for same prefix" $
        QC.forAll genValidPrefix $ \pfx ->
        QC.forAll genValidSuffix $ \sfx ->
          shouldTriggerHole (TriggerPrefix pfx) (mkTriggeredHoleName pfx sfx)
    ]


-- Generators
-------------

genTriggerPolicy :: QC.Gen TriggerPolicy
genTriggerPolicy =
  QC.oneof
    [ pure TriggerAll
    , pure TriggerNone
    , TriggerPrefix <$> genValidPrefix
    ]

genValidPrefix :: QC.Gen Text
genValidPrefix = do
  c0 <- QC.elements ['a' .. 'z']
  rest <- QC.listOf genIdentifierContinueChar
  pure (T.pack (c0 : rest))

genValidSuffix :: QC.Gen Text
genValidSuffix =
  T.pack <$> QC.listOf genIdentifierContinueChar

genDistinctSuffixPair :: QC.Gen (Text, Text)
genDistinctSuffixPair =
  QC.suchThat
    ((,) <$> genValidSuffix <*> genValidSuffix)
    (uncurry (/=))

genHoleName :: QC.Gen Text
genHoleName =
  QC.oneof
    [ pure "_"
    , mkTriggeredHoleName <$> genValidPrefix <*> genValidSuffix
    , T.pack <$> QC.listOf1 QC.arbitraryASCIIChar
    ]

genIdentifierContinueChar :: QC.Gen Char
genIdentifierContinueChar =
  QC.frequency
    [ (10, QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))
    , (1, pure '_')
    , (1, pure '\'')
    ]