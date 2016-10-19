{-# LANGUAGE OverloadedStrings #-}

module Text.Spintax (spintax) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import qualified Data.List.Extra      as E
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           System.Random.MWC

-- | Generate random texts based on a spinning syntax template, with nested alternatives and empty options.
--
-- >λ> spintax {{Oh my God|Awesome}, {a|the}|A|The} {quick {and dirty |||}||}{brown |pink |grey |}{fox|flea|elephant} jumps over {the|a} {lazy |smelly |sleepy |}{dog|cat|whale}{.|!|...}
-- > Right "Awesome, the quick pink fox jumps over a sleepy whale."
--
spintax :: T.Text -> IO (Either T.Text T.Text)
spintax template =
  createSystemRandom >>= flip runParse template
    where
      runParse g' i' = go g' "" [] i' (0::Int)
        where
          go g o as i l
            | l < 0  = failure
            | l == 0 =
              case parse spinSyntax i of
                Done r m  ->
                  case m of
                    "{" -> go g o as r (l+1)
                    "}" -> failure
                    "|" -> failure
                    _   -> go g (o <> m) as r l
                Partial _ -> return $ Right $ o <> i
                Fail {}   -> failure
            | l == 1 =
              case parse spinSyntax i of
                Done r m ->
                  case m of
                    "{" -> go g o (add as m) r (l+1)
                    "}" -> do r' <- runParse g =<< randAlter g as
                              case r' of
                                Left _ -> failure
                                Right t -> go g (o <> t) [] r (l-1)
                    "|" -> if E.null as
                             then go g o ["",""] r l
                             else go g o (E.snoc as "") r l
                    _   -> go g o (add as m) r l
                Partial _ -> failure
                Fail {} -> failure
            | l > 1 =
              case parse spinSyntax i of
                Done r m ->
                  case m of
                    "{" -> go g o (add as m) r (l+1)
                    "}" -> go g o (add as m) r (l-1)
                    _   -> go g o (add as m) r l
                Partial _ -> failure
                Fail {} -> failure
            where
              add _l _t =
                case E.unsnoc _l of
                  Just (xs,x) -> E.snoc xs $ x <> _t
                  Nothing     -> [_t]
              randAlter _g _as =
                (\r -> (!!) as (r-1)) <$> uniformR (1,E.length _as) _g
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
          go _ _ _ _ _ = failure

failure :: IO (Either T.Text b)
failure = return $ Left "Spintax template parsing failure"

