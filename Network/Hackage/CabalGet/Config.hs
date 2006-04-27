-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalGet.Config
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling saved state such as known packages, known servers and downloaded packages.
-----------------------------------------------------------------------------
module Network.Hackage.CabalGet.Config
    ( packagesDirectoryName
    , getKnownServers
    , getKnownPackages
    , writeKnownPackages
    , selectValidConfigDir
    ) where

import Control.Monad.Error (mplus, filterM) -- Using Control.Monad.Error to get the Error instance for IO.
import System.Directory (Permissions (..), getPermissions)

import Distribution.Package (PackageIdentifier)
import Distribution.Version (Dependency)
import Distribution.Compat.FilePath (joinFileName)

import Network.Hackage.CabalGet.Types (ConfigFlags (..))

pkgListFile :: FilePath
pkgListFile = "pkg.list"

servListFile :: FilePath
servListFile = "serv.list"

-- |Name of the packages directory.
packagesDirectoryName :: FilePath
packagesDirectoryName = "packages"

pkgList :: ConfigFlags -> FilePath
pkgList cfg = configConfPath cfg `joinFileName` pkgListFile

servList :: ConfigFlags -> FilePath
servList cfg = configConfPath cfg `joinFileName` servListFile

-- |Read the list of known packages from the pkg.list file.
getKnownPackages :: ConfigFlags -> IO [(PackageIdentifier,[Dependency],String)]
getKnownPackages cfg
    = fmap read (readFile (pkgList cfg))
      `mplus` return []

-- |Write the list of known packages to the pkg.list file.
writeKnownPackages :: ConfigFlags -> [(PackageIdentifier,[Dependency],String)] -> IO ()
writeKnownPackages cfg pkgs
    = writeFile (pkgList cfg) (show pkgs)

getKnownServers :: ConfigFlags -> IO [String]
getKnownServers cfg
    = fmap read (readFile (servList cfg))
      `mplus` return []

-- |Confirms validity of a config directory by checking the permissions for the package-list file,
--  server-list file and downloaded packages directory.
isValidConfigDir :: FilePath -> IO Bool
isValidConfigDir path
    = do checks <- sequence
                   [ checkFiles readable [ path
                                         , path `joinFileName` servListFile ]
                   , checkFiles writable [ path
                                         , path `joinFileName` pkgListFile
                                         , path `joinFileName` packagesDirectoryName ]]
         return (and checks)

-- |Picks the first valid config directory or throws an exception if none were found.
selectValidConfigDir :: [FilePath] -> IO FilePath
selectValidConfigDir paths
    = do valids <- filterM isValidConfigDir paths
         case valids of
           []    -> error "No valid config dir found!"
           (x:_) -> return x

checkFiles :: (Permissions -> Bool) -> [FilePath] -> IO Bool
checkFiles check
    = worker
    where worker [] = return True
          worker (x:xs)
              = do permissions <- getPermissions x
                   if check permissions
                      then worker xs
                      else return False
              `mplus` worker xs
