module Whine.Runner.PackageVersion where

import Whine.Runner.Prelude

import Parsing as P
import Parsing.String as PS
import Parsing.String.Basic as PB

data Version
  = ExactVersion VersionNumber
  | VersionRange { from :: VersionNumber, to :: VersionNumber }

type VersionNumber = { major :: Int, minor :: Int, patch :: Int }

formatVersion :: Version -> String
formatVersion = case _ of
  ExactVersion v -> formatVersionNumber v
  VersionRange r -> formatVersionRange r

formatVersionNumber :: VersionNumber -> String
formatVersionNumber { major, minor, patch } = fold [show major, ".", show minor, ".", show patch]

formatVersionRange :: { from :: VersionNumber, to :: VersionNumber } -> String
formatVersionRange { from, to } = ">=" <> formatVersionNumber from <> " <" <> formatVersionNumber to

versionToRange :: Version -> { from :: VersionNumber, to :: VersionNumber }
versionToRange = case _ of
  ExactVersion v -> { from: v, to: v { patch = v.patch + 1 } }
  VersionRange r -> r

parseVersion :: String -> Either String Version
parseVersion v = P.runParser v versionParser # lmap (const "Malformed package version")

versionParser :: P.Parser String Version
versionParser = exact <|> range
  where
    exact = ExactVersion <$> versionNumber

    range = do
      void $ PS.string ">="
      from <- versionNumber
      void $ PS.string "<"
      to <- versionNumber
      pure $ VersionRange { from, to }

    versionNumber :: P.Parser String VersionNumber
    versionNumber = do
      major <- PB.intDecimal
      void $ PS.char '.'
      minor <- PB.intDecimal
      void $ PS.char '.'
      patch <- PB.intDecimal

      if major < 0 || minor < 0 || patch < 0 then
        P.fail ""
      else
        pure { major, minor, patch }
