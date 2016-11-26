{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Spintax (spintax) where

import           Control.Applicative  ((<|>))
import           Control.Monad.Reader (runReaderT, ask)
import           Data.Attoparsec.Text
import qualified Data.List.Extra      as E
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           System.Random.MWC

-- | Generate random texts based on a spinning syntax template, with nested alternatives and empty options.
--
-- >λ> spintax "{{Oh my God|Awesome}, {a|the}|A|The} {quick {and dirty |||}||}{brown |pink |grey |}{fox|flea|elephant} jumps over {the|a} {lazy |smelly |sleepy |}{dog|cat|whale}{.|!|...}"
-- > Right "Awesome, the quick pink fox jumps over a sleepy whale."
--
spintax :: T.Text -> IO (Either String T.Text)
spintax template =
  createSystemRandom >>= runReaderT (spin template)
  where
    spin t = go T.empty [] t (0::Int)
      where
        go o as i l
          | l < 0  = parseFail
          | l == 0 =
            case parse spinSyntax i of
              Done r m  ->
                case m of
                  "{" -> go o as r (l+1)
                  "}" -> parseFail
                  "|" -> parseFail
                  _   -> go (o <> m) as r l
              Partial _ -> return $ Right $ o <> i
              Fail {}   -> parseFail
          | l == 1 =
            case parse spinSyntax i of
              Done r m ->
                case m of
                  "{" -> go o (add as m) r (l+1)
                  "}" -> do
                    s <- spin =<< randAlter as =<< ask
                    case s of
                      Left _   -> parseFail
                      Right t' -> go (o <> t') [] r (l-1)
                  "|" ->
                    if E.null as
                      then go o ["",""] r l
                      else go o (E.snoc as "") r l
                  _   -> go o (add as m) r l
              Partial _ -> parseFail
              Fail {} -> parseFail
          | l > 1 =
            case parse spinSyntax i of
              Done r m ->
                case m of
                  "{" -> go o (add as m) r (l+1)
                  "}" -> go o (add as m) r (l-1)
                  _   -> go o (add as m) r l
              Partial _ -> parseFail
              Fail {}   -> parseFail
          where
            add _l _t =
              case E.unsnoc _l of
                Just (xs,x) -> E.snoc xs $ x <> _t
                Nothing     -> [_t]
            randAlter _as _g =
              (\r -> (!!) as (r-1)) <$> uniformR (1,E.length _as) _g
        go _ _ _ _ = parseFail 
        parseFail = fail msg

spinSyntax :: Parser T.Text
spinSyntax =
  openBrace <|> closeBrace <|> pipe <|> content
    where
      openBrace = string "{"
      closeBrace = string "}"
      pipe = string "|"
      content =
        takeWhile1 ctt
          where
            ctt '{' = False
            ctt '}' = False
            ctt '|' = False
            ctt _   = True

msg :: String
msg = "Spintax template parsing failure"

