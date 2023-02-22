{-# LANGUAGE CPP #-}
module Main (main) where

{- TODO: Update documentation
  This executable can generate CBOR encodings of the NFT minting policy and
  the pool validators. It takes the necessary arguments from the CLI to
  generate them. The resulting scripts are *fully* applied.

  Some examples:

  cabal exec serialise -- state-nft <Transaction Hash> <Output Index>
  cabal exec serialise -- list-nft <Transaction Hash> <Output Index> <Public Key Hash>
  cabal exec serialise -- validator <Transaction Hash> <Output Index> <Public Key Hash>

  Use -v to print to console the policy/validator hash and -o to choose a
  different output file. The latter option can be used with /dev/stdout to print
  the result to screen.
-}

import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ParserInfo,
  command,
  execParser,
  fullDesc,
  help,
  info,
  long,
  metavar,
  progDesc,
  short,
  showDefault,
  subparser,
 )
import Plutus.V1.Ledger.Scripts (Script)

import BondedStaking.BondedPool (pbondedPoolValidatorUntyped)
import ListNFT (plistNFTPolicyUntyped)
import Plutarch (compile)
import SingularityNet.Settings (bondedStakingTokenName, unbondedStakingTokenName)
import StateNFT (pstateNFTPolicyUntyped)
#ifdef TimeChecks
import UnbondedStaking.UnbondedPool (punbondedPoolValidatorUntyped)
#else
import UnbondedStaking.UnbondedPoolDebug (punbondedPoolValidatorUntyped)
#endif
import Options.Applicative.Builder (strOption)

serialisePlutusScript :: Script -> Text
serialisePlutusScript script = encodeToLazyText script

writeScriptsToFile ::
  FilePath ->
  [(Text, Script)] ->
  IO ()
writeScriptsToFile filepath nameScripts =
  TIO.writeFile filepath $
    "// File generated automatically by serialise\n"
      <> T.concat (fmap (\(name, script) ->
         "exports._" <> name <> " = {\n"
           <> "\tscript: "
           <> serialisePlutusScript script
           <> ",\n};\n"
         ) nameScripts)

main :: IO ()
main = do
  args <- execParser opts
  case cliCommand args of
    SerialiseStateNFT -> do
      writeScriptsToFile
        (outPath args)
        [("bondedStateNFT", compile $ pstateNFTPolicyUntyped bondedStakingTokenName)
         , ("unbondedStateNFT", compile $ pstateNFTPolicyUntyped unbondedStakingTokenName)]
    SerialiseListNFT -> do
      writeScriptsToFile
        (outPath args)
        [("listNFT", compile plistNFTPolicyUntyped)]
    SerialiseValidator -> do
      writeScriptsToFile
        (outPath args)
        [("bondedPoolValidator", compile pbondedPoolValidatorUntyped)
        ,("unbondedPoolValidator", compile punbondedPoolValidatorUntyped)]

-- Parsers --
data CLI = CLI
  { outPath :: FilePath
  , cliCommand :: CLICommand
  }

data CLICommand
  = SerialiseStateNFT
  | SerialiseListNFT
  | SerialiseValidator

opts :: ParserInfo CLI
opts =
  info
    parser
    ( fullDesc
        <> progDesc "Serialise a NFT policy or the pool validators"
    )

parser :: Parser CLI
parser =
  CLI
    <$> outOption
    <*> commandParser

commandParser :: Parser CLICommand
commandParser =
  subparser $
    serialiseStateNFTCommand
      <> serialiseListNFTCommand
      <> serialiseValidatorCommand

serialiseStateNFTCommand :: Mod CommandFields CLICommand
serialiseStateNFTCommand =
  command "state-policy" $
    info
      (pure SerialiseStateNFT)
      ( fullDesc
          <> progDesc
            "Serialise the NFT minting policy of the pools"
      )

serialiseListNFTCommand :: Mod CommandFields CLICommand
serialiseListNFTCommand =
  command "list-policy" $
    info
      (pure SerialiseListNFT)
      ( fullDesc
          <> progDesc
            "Serialise the NFT minting policy of the association list"
      )

serialiseValidatorCommand :: Mod CommandFields CLICommand
serialiseValidatorCommand =
  command "validator" $
    info
      (pure SerialiseValidator)
      ( fullDesc
          <> progDesc
            "Serialise the pool validators"
      )

outOption :: Parser FilePath
outOption =
  strOption
    ( long "out"
        <> short 'o'
        <> metavar "OUT"
        <> help "Path of serialised file"
        <> showDefault
    )
