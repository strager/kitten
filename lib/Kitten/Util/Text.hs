{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Kitten.Util.Text
  ( Textable(..)
  , ToText(..)
  , indent
  , indentSpaces
  , readFileUtf8
  , replicateChar
  , showText
  , module Data.Text
  ) where

import Control.Applicative
import Data.Text
import Data.Text.Encoding

import qualified Data.ByteString as B
import qualified Data.Text as Text

data Textable = forall a. (ToText a) => Textable a

instance ToText Textable where
  toText (Textable x) = toText x

class ToText a where
  toText :: a -> Text

indent :: Text -> Text
indent = indentSpaces 2

indentSpaces :: Int -> Text -> Text
indentSpaces level
  = Text.unlines . Prelude.map (Text.append spaces) . Text.lines
  where
  spaces :: Text
  spaces = replicateChar level ' '

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = decodeUtf8 <$> B.readFile path

replicateChar :: Int -> Char -> Text
replicateChar n c
  = Text.replicate n (Text.singleton c)

showText :: (Show a) => a -> Text
showText = Text.pack . show
