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
import Distribution.Client.Setup (globalCommand)

import Data.Char (toUpper)
import Data.List (intercalate)

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
  concatMap (commandSynopsisLines pname) commands ++
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
  , "Global options:"
  , ""
  ] ++
  optionsLines (globalCommand []) ++
  [ ".SH COMMANDS"
  ] ++
  concatMap (commandDetailsLines pname) commands ++
  [ ".SH FILES"
  , "TODO: .cabal/config"
  , ".SH BUGS"
  , "TODO"
  , ".SH SEE ALSO"
  , "TODO"
  , ".SH LICENSE"
  , "TODO"
  ]

commandSynopsisLines :: String -> CommandSpec action -> [String]
commandSynopsisLines pname (CommandSpec ui _ Visible) =
  [ ".B " ++ pname ++ " " ++ (commandName ui)
  , ".R - " ++ commandSynopsis ui
  , ".br"
  ]
commandSynopsisLines _ (CommandSpec _ _ Hidden) = []

commandDetailsLines :: String -> CommandSpec action -> [String]
commandDetailsLines pname (CommandSpec ui _ Visible) =
  [ ".B " ++ pname ++ " " ++ (commandName ui) 
  , ""
  , commandUsage ui pname
  , ""
  ] ++
  optional commandDescription ++
  optional commandNotes ++
  [ "Flags:"
  , ".RS"
  ] ++
  optionsLines ui ++
  [ ".RE"
  , "" 
  ]
  where
    optional field =
      case field ui of
        Just text -> [text pname, ""]
        Nothing   -> []
commandDetailsLines _ (CommandSpec _ _ Hidden) = []

optionsLines :: CommandUI flags -> [String]
optionsLines command = concatMap optionLines (concatMap optionDescr (commandOptions command ParseArgs))

data ArgumentRequired = Optional | Required
type OptionArg = (ArgumentRequired, ArgPlaceHolder)  

optionLines :: OptDescr flags -> [String]
optionLines (ReqArg description (optionChars, optionStrings) placeHolder _ _) = 
  standardOptionLines description optionChars optionStrings (Required, placeHolder)
optionLines (OptArg description (optionChars, optionStrings) placeHolder _ _ _) =
  standardOptionLines description optionChars optionStrings (Optional, placeHolder)
optionLines (BoolOpt description (trueChars, trueStrings) _ _ _) =
  [ optionsLine trueChars trueStrings ] ++
  optionDescriptionLines description
optionLines (ChoiceOpt options) =
  concatMap choiceLines options
  where
    choiceLines (description, (optionChars, optionStrings), _, _) =
      [ optionsLine optionChars optionStrings ] ++
      optionDescriptionLines description

standardOptionLines :: String -> [Char] -> [String] -> OptionArg -> [String]
standardOptionLines description optionChars optionStrings arg =
  [ optionsLine optionChars optionStrings
  , optionArgLine arg
  ] ++
  optionDescriptionLines description

optionDescriptionLines :: String -> [String]
optionDescriptionLines description =
  [ ".RS"
  , description
  , ".RE"
  , ""
  ]

optionsLine :: [Char] -> [String] -> String
optionsLine optionChars optionStrings =
  intercalate ", " (shortOptions optionChars ++ longOptions optionStrings)

shortOptions :: [Char] -> [String]
shortOptions = map (\c -> "\\-" ++ [c])

longOptions :: [String] -> [String]
longOptions = map (\s -> "\\-\\-" ++ s)

optionArgLine :: OptionArg -> String
optionArgLine (Required, placeHolder) = ".I " ++ placeHolder
optionArgLine (Optional, placeHolder) = ".RI [ " ++ placeHolder ++ " ]"
