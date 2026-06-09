{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text.IO        as T
import           Options.Applicative (execParser)
import           System.Exit         (exitFailure)
import           Text.Spintax        (spintax)

import           Options

main :: IO ()
main = do
  Options{..} <- execParser opts
  if showver
    then putStrLn showVer >> exitFailure
    else case template of
      Nothing  -> putStrLn "Missing: (-t|--template ARG)" >> exitFailure
      Just t -> do
        r <- spintax t
        case r of
          Left  e -> putStrLn e >> exitFailure
          Right s -> T.putStrLn s
