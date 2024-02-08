{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import ClassyPrelude          ( IO, error, interact )
import Control.Applicative    ( (<**>), many )
import Data.Function          ( ($) )
import Data.Function.Unicode  ( (∘) )
import Data.Map.Strict        ( foldrWithKey, fromListWithKey )
import Data.Monoid.Unicode    ( (⊕) )
import Data.Text              ( replace, unpack )
import Data.Text.Lazy         ( fromStrict, toStrict )
import Options.Applicative    ( execParser, helper, info, metavar, progDesc
                              , strArgument )

main ∷ IO ()
main = do
  let desc   = progDesc "filter stdin→stdout to make textual replacements"
      parser = many $ strArgument (metavar "FROM TO") <**> helper
  argv ← execParser $ info parser desc
  let pairs []  = []
      pairs [k] = error $ "uneven pairs (last: '" ⊕ unpack k ⊕ "')"
      pairs (k : v : xs) = (k,v) : pairs xs
  let args = pairs argv
      combiner k _ _ = error $ "repeated key '" ⊕ unpack k ⊕ "'"
  let replacements = fromListWithKey combiner args
  let go t = foldrWithKey replace t replacements

  interact (fromStrict ∘ go ∘ toStrict)
