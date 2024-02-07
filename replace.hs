{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import ClassyPrelude    ( IO, error, getArgs, interact, print )
import Data.Function    ( ($), (.) )
import Data.Functor     ( fmap )
import Data.Map.Strict  ( foldrWithKey, fromListWithKey )
import Data.Semigroup   ( (<>) )
import Data.Text        ( breakOn, drop, replace, unpack )
import Data.Text.Lazy   ( fromStrict, toStrict )

main ∷ IO ()
main = do
  argv ← getArgs
  let splitter s = case breakOn "=" s of
                     (_,"") → error $ "no '=' found in  '" <> unpack s <> "'"
                     (from,to) → (from,drop 1 to)
      args = fmap splitter argv
      combiner k _ _ = error $ "repeated key '" <> unpack k <> "'"
  let replacements = fromListWithKey combiner args
  let go t = foldrWithKey replace t replacements

  interact (fromStrict . go . toStrict)
