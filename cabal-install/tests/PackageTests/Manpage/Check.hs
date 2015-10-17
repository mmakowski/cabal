{-# LANGUAGE CPP #-}
module PackageTests.Manpage.Check
   ( tests
   ) where

import PackageTests.PackageTester (cabal, packageTestsDirectory, TestsPaths)

import Control.Monad (unless, when)
import Test.Tasty
import Test.Tasty.HUnit

import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))

dir :: FilePath
dir = packageTestsDirectory </> "Manpage"

tests :: TestsPaths -> [TestTree]
tests paths =
  [ testCase "outputs manpage source" $ do
      (_, exitCode, output) <- cabal paths dir ["manpage"]
      exitCode @?= ExitSuccess
      let outputLines = lines output
      -- starts with header line:
      head outputLines @?= ".TH CABAL 1"
      -- visible commands are included:
      outputLines `assertContains` ".B cabal install"
      -- hidden commands are not included:
      outputLines `assertDoesNotContain` ".B cabal manpage"
  ]

assertContains :: (Eq a, Show a) => [a] -> a -> Assertion
assertContains xs x = unless (elem x xs) (assertFailure (show x ++ " not found in " ++ show xs))

assertDoesNotContain :: (Eq a, Show a) => [a] -> a -> Assertion
assertDoesNotContain xs x = when (elem x xs) (assertFailure (show x ++ " found in " ++ show xs))
