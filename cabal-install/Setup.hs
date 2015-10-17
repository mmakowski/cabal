import Distribution.PackageDescription ( PackageDescription )
import Distribution.Simple ( defaultMainWithHooks
                           , simpleUserHooks
                           , postBuild
                           , postCopy
                           , postInst
                           )
import Distribution.Simple.InstallDirs ( mandir
                                       , CopyDest (NoCopyDest)
                                       )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..)
                                          , absoluteInstallDirs
                                          )
import Distribution.Simple.Utils ( copyFiles )
import Distribution.Simple.Setup ( copyDest
                                 , copyVerbosity
                                 , fromFlag
                                 , installVerbosity
                                 )
import Distribution.Verbosity ( Verbosity )

import System.IO ( openFile
                 , IOMode (WriteMode)
                 )
import System.Posix.Files ( groupReadMode
                          , otherReadMode
                          , ownerReadMode
                          , ownerWriteMode
                          , setFileMode
                          , unionFileModes
                          )
import System.Posix.Types ( FileMode )
import System.Process ( runProcess )
import System.FilePath ( (</>) )


main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks 
  { postBuild = \ _ _ _ lbi -> 
      buildManpage lbi
  , postCopy = \ _ flags pkg lbi ->
      installManpage pkg lbi (fromFlag $ copyVerbosity flags) (fromFlag $ copyDest flags)
  , postInst = \ _ flags pkg lbi ->
      installManpage pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest
  }
   
buildManpage :: LocalBuildInfo -> IO ()
buildManpage lbi = do
  let cabal = buildDir lbi </> "cabal/cabal"
      manpage = buildDir lbi </> "cabal/cabal.1"
  manpageHandle <- openFile manpage WriteMode
  runProcess cabal ["manpage"] Nothing Nothing Nothing (Just manpageHandle) Nothing
  return ()

installManpage :: PackageDescription -> LocalBuildInfo -> Verbosity -> CopyDest -> IO ()
installManpage pkg lbi verbosity copy = do
  let destDir = mandir (absoluteInstallDirs pkg lbi copy) </> "man1"
  copyFiles verbosity destDir [(buildDir lbi </> "cabal", "cabal.1")]
  setFileMode (destDir </> "cabal.1") mode644

mode644 :: FileMode
mode644 = ownerReadMode  `unionFileModes`
          ownerWriteMode `unionFileModes`
          groupReadMode  `unionFileModes`
          otherReadMode
