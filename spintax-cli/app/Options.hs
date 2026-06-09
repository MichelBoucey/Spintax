{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Options where

#if !MIN_VERSION_base(4,17,0)
import           Control.Applicative (optional)
#endif

import qualified Data.Text           as T
import           Data.Version        (showVersion)
import           Options.Applicative
import           Paths_spintax_cli   (version)

data Options = Options
  { template :: Maybe T.Text
  , showver  :: Bool
  }

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
    <> progDesc "Spintax random text generator"
    <> header ( "spintax "
                <> showVer
                <> ", (c) Michel Boucey 2016-2026" ) )

options :: Parser Options
options =
  Options
    <$> optional (fmap T.pack
         (strOption
           ( short 't'
             <> long "template"
             <> help "Spintax template string" )))
    <*> flag False True
         ( short 'v'
           <> long "version"
           <> help "Show version" )

showVer :: String
showVer = "v" <> showVersion version
