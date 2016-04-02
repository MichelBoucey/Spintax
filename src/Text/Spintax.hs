{-# LANGUAGE OverloadedStrings #-}

module Text.Spintax where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import qualified Data.List.Extra      as E
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           System.Random.MWC

-- | Generate random texts based on a spinning syntax template, with nested alternatives and empty options.
--
-- >λ> spintax "{A|B|C|{a|b|c{1|2|3}|d}|D}{|, {..|etc}.}"
-- > Right "c2"
--
spintax :: T.Text -> IO (Either T.Text T.Text)
spintax template =
    createSystemRandom >>= flip runParse template
  where
    runParse gen' input' = getText gen' "" [] input' (0::Int)
      where
        getText gen output alters input nestlev
            | nestlev < 0  = failure
            | nestlev == 0 =
                case parse spinSyntax input of
                    Done rest _match ->
                        case _match of
                            "{" -> getText gen output alters rest (nestlev+1)
                            "}" -> failure
                            "|" -> failure
                            _   -> getText gen (output <> _match) alters rest nestlev
                    Partial _ -> return $ Right $ output <> input
                    Fail {} -> failure
            | nestlev == 1 =
                case parse spinSyntax input of
                    Done rest _match ->
                        case _match of
                            "{" -> getText gen output (addToLast alters _match) rest (nestlev+1)
                            "}" -> do result <- runParse gen =<< randAlter gen alters
                                      case result of
                                          Left _ -> failure
                                          Right text -> getText gen (output <> text) [] rest (nestlev-1)
                            "|" -> if E.null alters
                                    then getText gen output ["",""] rest nestlev
                                    else getText gen output (E.snoc alters "") rest nestlev
                            _   -> getText gen output (addToLast alters _match) rest nestlev
                    Partial _ -> failure
                    Fail {} -> failure
            | nestlev > 1 =
                case parse spinSyntax input of
                    Done rest _match ->
                        case _match of
                            "{" -> getText gen output (addToLast alters _match) rest (nestlev+1)
                            "}" -> getText gen output (addToLast alters _match) rest (nestlev-1)
                            _   -> getText gen output (addToLast alters _match) rest nestlev
                    Partial _ -> failure
                    Fail {} -> failure
          where
            addToLast l t =
                case E.unsnoc l of
                    Just (xs,x) -> E.snoc xs $ x <> t
                    Nothing     -> [t]
            randAlter _g as = uniformR (1,E.length as) _g >>= \r -> return $ (!!) as (r-1)
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
        getText _ _ _ _ _ = failure

failure :: IO (Either T.Text b)
failure = return $ Left "Spintax template parsing failure"

