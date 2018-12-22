{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import Data.CMake.Types
import Data.CMake.Types.Targets
import Data.CMake.Types.Targets.Executable
import Data.CMake.Types.Targets.IncludeDirectory
import Data.Either (either, isLeft)
import Data.Text
import Test.Hspec

import qualified Data.ByteString as ByteString
import qualified Data.Yaml as Yaml

shouldBeLeft :: Yaml.FromJSON v => v -> Either a v -> Expectation
shouldBeLeft _ = shouldBe True . isLeft

expectLeft :: Yaml.FromJSON v => v -> ByteString -> Expectation
expectLeft v s = (shouldBeLeft v) (Yaml.decodeEither' s)

withRightExpect :: Yaml.FromJSON a
                => Either Yaml.ParseException a
                -> (a -> Expectation)
                -> Expectation
withRightExpect result f = either (expectationFailure . show) f result

expectThatRight :: Yaml.FromJSON a => ByteString -> (a -> Expectation) -> Expectation
expectThatRight = withRightExpect . Yaml.decodeEither'

main :: IO ()
main = do

  hspec $ do

    describe "project:" $ do

      -- name: test
      -- version: 0.1.3

      describe "name: test" $ do

        it "should set name to Just \"test\"" $
          expectThatRight "name: test\nversion: 0.1.3" $
            \config -> name (project config) `shouldBe` Just "test"

      -- name: [1]
      -- version: 0.1.3

      describe "name: [1]" $ do

        it "should fail to parse" (expectLeft (undefined :: Config) "name: [1]\nversion: 0.1.3")

      -- version: 0.1.3

      describe "name: -" $ do

        it "should set name to Nothing" $
          expectThatRight "version: 0.1.3" $
            \config -> name (project config) `shouldBe` Nothing

      -- name: test
      -- languages:
      --   - CXX
      --   - Fortran

      describe "languages: [CXX, Fortran]" $

        it "should set languages to [\"CXX\", \"Fortran\"]" $
          expectThatRight "name: test\nlanguages:\n  - CXX\n  - Fortran" $
            \config -> languages (project config) `shouldBe` ["CXX", "Fortran"]

      -- name: test
      -- languages: OCaml

      describe "languages: OCaml" $ do

        it "should set languages to [\"OCaml\"]" $
          expectThatRight "name: test\nlanguages: OCaml" $
           \config -> languages (project config) `shouldBe` ["OCaml"]

      -- name: test
      -- languages: 3

      describe "languages: 3" $ do

        it "should fail to parse" (expectLeft (undefined :: Config) "name: test\nlanguages: 3")

      -- name: test
      -- languages: []

      describe "languages: []" $ do

        it "should set languages to []" $
          expectThatRight "name: test\nlanguages: []" $
            \config -> languages (project config) `shouldBe` []

      -- name: test

      describe "languages: -" $ do

        it "should set languages to []" $
          expectThatRight "name: test" $
            \config -> languages (project config) `shouldBe` []

      -- {}

      describe "empty config" $ do

        it "should set name to Nothing" $
          expectThatRight "{}" $
            \config -> name (project config) `shouldBe` Nothing

      -- ""

      describe "null config" $ do

        it "should set name to Nothing" $
          expectThatRight "" $
            \config -> name (project config) `shouldBe` Nothing

    describe "executables:" $ do

      -- name: test
      -- executables:
      --   main:
      --     includeDirectories:
      --     - include/bananas
      --     - include/apples
      --     - include/oranges

      describe "name: test\nexecutables:\n  main:\n    includeDirectories:\n    - include/bananas\n    - include/apples\n    - include/oranges" $ do

        it "should be a list" $
          expectThatRight "name: test\nexecutables:\n  main:\n    includeDirectories:\n    - include/bananas\n    - include/apples\n    - include/oranges" $
            \config -> Prelude.head (executables (targets config))
              `shouldBe` ("main", Executable [ IncludeDirectory "include/bananas" "public"
                                             , IncludeDirectory "include/apples"  "public"
                                             , IncludeDirectory "include/oranges" "public" ])

      -- name: test
      -- executables:
      --   main:
      --     includeDirs:
      --     - include/bananas
      --     - include/apples
      --     - include/oranges

      describe "name: test\nexecutables:\n  main:\n    includeDirs:\n    - include/bananas\n    - include/apples\n    - include/oranges" $ do

        it "should be a list" $
          expectThatRight "name: test\nexecutables:\n  main:\n    includeDirs:\n    - include/bananas\n    - include/apples\n    - include/oranges" $
            \config -> Prelude.head (executables (targets config))
              `shouldBe` ("main", Executable [ IncludeDirectory "include/bananas" "public"
                                             , IncludeDirectory "include/apples"  "public"
                                             , IncludeDirectory "include/oranges" "public" ])

      -- name: test
      -- executables:
      --   main:
      --     files:
      --     - src/foo.cpp
      --     - src/baz.cpp
      --     - src/moo.cpp

      describe "name: test\nexecutables:\n  main:\n    files:\n    - src/foo.cpp\n    - src/baz.cpp\n    - src/moo.cpp" $ do

        it "should be a list" $
          expectThatRight "name: test\nexecutables:\n  main:\n    files:\n    - src/foo.cpp\n    - src/baz.cpp\n    - src/moo.cpp" $
            \config -> Prelude.head (executables (targets config))
              `shouldBe` ("main", Executable [])

      -- name: test
      -- executables:
      --   main:
      --     includeDirs:
      --       - path: include/apples
      --         scope: public
      --       - path: include/oranges
      --         scope: private

      describe "name: test\nexecutables:\n  main:\n    includeDirs:\n      - path: include/apples\n        scope: public\n      - path: include/oranges\n        scope: private" $ do

        it "should be a list" $
          expectThatRight "name: test\nexecutables:\n  main:\n    includeDirs:\n      - path: include/apples\n        scope: public\n      - path: include/oranges\n        scope: private" $
            \config -> Prelude.head (executables (targets config))
              `shouldBe` ("main", Executable [ IncludeDirectory "include/apples"  "public"
                                             , IncludeDirectory "include/oranges" "private" ])

      -- name: test
      -- executables:
      --   main:
      --     includeDirectories: 3

      describe "name: test\nexecutables:\n  main:\n    includeDirectories: 3" $ do

        it "should fail to parse" $
          expectLeft (undefined :: Config) "name: test\nexecutables:\n  main:\n    includeDirectories: 3"

      -- name: test
      -- executables:
      --   main:
      --     includeDirs: 3

      describe "name: test\nexecutables:\n  main:\n    includeDirs: 3" $ do

        it "should fail to parse" $
          expectLeft (undefined :: Config) "name: test\nexecutables:\n  main:\n    includeDirs: 3"

--    describe "libraries:" $ do
--    describe "cmake:" $ do
--    describe "variables:" $ do
--    describe "install:" $ do
