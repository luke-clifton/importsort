{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Attoparsec.Text
import Data.Text as T
import Data.Text.IO as T
import Data.List
import Data.Char
import Text.Printf

main :: IO ()
main = T.interact fmtInputs

data Import = Import
    { mod :: Text         -- Sort by module
    , qualified :: Bool   -- Then by qualified, makes scanning easier.
    } deriving (Eq, Ord, Show)

fmtInputs :: Text -> Text
fmtInputs = T.unlines
    . sortOn pImport
    . T.lines

pImport :: Text -> Import
pImport t = case parseOnly parseImport t of
    Right i -> i
    Left e -> error e

parseImport :: Parser Import
parseImport = do
    skipSpace
    string "import"
    skipSpace
    qualified <- option False (True <$ string "qualified")
    skipSpace
    mod <- takeWhile1 (not . isSpace)
    return $ Import{..}
