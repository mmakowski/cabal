{-# LANGUAGE CPP, ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Manpage
-- Copyright   :  (c) Maciek Makowski 2015
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  TODO MM: provisional?
-- Portability :  TODO MM: portable?
--
-- Supporting functions for building the manual page.

module Distribution.Client.Manpage
  ( CommandVisibility (..)
  , CommandSpec (..)
  , commandFromSpec
  , manpage
  ) where

import Distribution.Simple.Command

import Data.Char (toUpper)

data CommandVisibility = Visible | Hidden

-- TODO MM: docs
data CommandSpec action = forall flags. CommandSpec (CommandUI flags) (CommandUI flags -> Command action) CommandVisibility

commandFromSpec :: CommandSpec a -> Command a
commandFromSpec (CommandSpec ui action _) = action ui

manpage :: String -> [CommandSpec a] -> String
manpage pname commands = unlines $
  [ ".TH " ++ map toUpper pname ++ " 1"
  , ".SH NAME"
  , pname ++ " \\- a system for building and packaging Haskell libraries and programs"
  , ".SH SYNOPSIS"
  , ".B " ++ pname
  , ".I command"
  , ".RI < arguments |[ options ]>..."
  , ""
  , "Where the"
  , ".I commands"
  , "are"
  , ""
  ] ++
  concatMap commandSynopsisLines commands ++
  [ ".SH DESCRIPTION"
  , "Cabal is the standard package system for Haskell software. It helps people to configure, " 
  , "build and install Haskell software and to distribute it easily to other users and developers."
  , ""
  , "The command line " ++ pname ++ " tool (also referred to as cabal-install) helps with "
  , "installing existing packages and also helps people developing their own packages. "
  , "It can be used to work with local packages or to install packages from online package archives, "
  , "including automatically installing dependencies. By default it is configured to use Hackage, "
  , "which is Haskell’s central package archive that contains thousands of libraries and applications "
  , "in the Cabal package format."
  , ".SH OPTIONS"
  , "TODO"
  , ".SH COMMANDS"
  ] ++
  concatMap commandDetailsLines commands ++
  [ ".SH BUGS"
  , "TODO"
  , ".SH SEE ALSO"
  , "TODO"
  , ".SH LICENSE"
  , "TODO"
  ]
  where
    commandSynopsisLines (CommandSpec ui _ Visible) =
      [ ".B " ++ pname ++ " " ++ (commandName ui)
      , ".R - " ++ commandSynopsis ui
      , ".br"
      ]
    commandSynopsisLines (CommandSpec _ _ Hidden) = []
    commandDetailsLines (CommandSpec ui _ Visible) =
      [ ".B " ++ pname ++ " " ++ (commandName ui)
      , "TODO"
      ]
    commandDetailsLines (CommandSpec _ _ Hidden) = []
