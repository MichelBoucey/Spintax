{-# LANGUAGE OverloadedStrings #-}

module Text.Spintax where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Either
import Data.List.Extra as E
import Data.Text as T
import System.Random.MWC

-- | Generate random texts based on a spinning syntax template, with nested alternatives and empty options.
--
-- >λ> spintax "{A|B|C|{a|b|c{1|2|3}|d}|D}{|, {..|etc}.}"
-- > Right "c2"
--
spintax :: T.Text -> IO (Either T.Text T.Text)
spintax template =
    createSystemRandom >>= \gen -> runParse gen template
  where
    runParse gen input =
        getText gen "" [] input 0
      where
        getText gen output alters input nestlev 
            | nestlev < 0  = return failure
            | nestlev == 0 =
                case parse spinSyntax input of
                    Done rest match ->
                        case match of
                            "{" -> getText gen output alters rest (nestlev+1)
                            "}" -> return failure
                            "|" -> return failure
                            _   -> getText gen (output `append` match) alters rest nestlev
                    Partial _ -> return $ Right $ output `append` input
                    Fail {} -> return failure
            | nestlev == 1 =
                case parse spinSyntax input of
                    Done rest match ->
                        case match of
                            "{" -> getText gen output (addToLast alters match) rest (nestlev+1)
                            "}" -> do result <- runParse gen =<< randAlter gen alters
                                      case result of
                                          Left _ -> return failure
                                          Right text -> getText gen (output `append` text) [] rest (nestlev-1)
                            "|" -> if E.null alters
                                    then getText gen output ["",""] rest nestlev
                                    else getText gen output (E.snoc alters "") rest nestlev
                            _   -> getText gen output (addToLast alters match) rest nestlev
                    Partial _ -> return failure
                    Fail {} -> return failure 
            | nestlev > 1 =
                case parse spinSyntax input of
                    Done rest match ->
                        case match of
                            "{" -> getText gen output (addToLast alters match) rest (nestlev+1)
                            "}" -> getText gen output (addToLast alters match) rest (nestlev-1)
                            _   -> getText gen output (addToLast alters match) rest nestlev
                    Partial _ -> return failure
                    Fail {} -> return failure
          where
            failure = Left "Spintax template parsing failure"
            addToLast l t =
                case E.unsnoc l of
                    Just (xs,x) -> E.snoc xs $ x `append` t
                    Nothing     -> [t]
            randAlter g as = uniformR (1,E.length as) g >>= \r -> return $ (!!) as (r-1)
            spinSyntax =
                openBrace <|> closeBrace <|> pipe <|> content
              where
                pipe = string "|"
                openBrace = string "{"
                closeBrace = string "}"
                content =
                    takeWhile1 ctt
                  where
                    ctt '{' = False
                    ctt '}' = False
                    ctt '|' = False
                    ctt _   = True

