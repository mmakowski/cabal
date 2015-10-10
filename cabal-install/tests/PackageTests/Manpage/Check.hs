{-# LANGUAGE CPP #-}
module PackageTests.Manpage.Check
   ( tests
   ) where

import PackageTests.PackageTester (cabal, packageTestsDirectory, TestsPaths)

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
      head (lines output) @?= ".TH CABAL 1 \"1.20.0.3\"" -- TODO: version
      -- TODO: expected output other lines
  ]
