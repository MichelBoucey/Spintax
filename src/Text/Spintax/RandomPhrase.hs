module Text.Spintax.RandomPhrase where

import qualified Data.Text    as T
import           Text.Spintax

type RandomPhrase = T.Text

-- | Generate random passphrase or unique id
--
-- >λ> randomPhrase "-" [["blacky","monk","gillespie","coltrane"],["apple","apricot","banana","coconut"],["kant","hegel","husserl","habermas"]]
-- > Right "coltrane-coconut-kant"
--
randomPhrase :: T.Text -> [[T.Text]] -> IO (Either String RandomPhrase)
randomPhrase s ls = spintax $ writeSpintaxExpression s $ writeSpintaxAlternative <$> ls

