{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Text.Template (Context)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Template as Template
import qualified Data.Text as T


context :: [(Text, Text)] -> Context
context kvs k = case lookup k kvs of
  Just v  -> v
  Nothing -> "/* fatal error in solpb: bad template */"

format :: Text -> [(Text, Text)] -> Text
format tmpl = toStrict . Template.substitute tmpl . context

stripLineEndings :: Text -> Text
stripLineEndings = T.unlines . map T.stripEnd . T.lines

show' :: Show a => a -> Text
show' = pack . show

