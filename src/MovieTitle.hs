#!/usr/bin/env stack
-- stack --resolver lts-7.5 --install-ghc runghc

-- A script to parse movie title from movie folder/file path.

module MovieTitle where

import System.Exit (exitFailure)
import System.IO
import Data.Char (isSpace)
import Control.Monad (replicateM)
import Text.ParserCombinators.Parsec
import Data.List.Utils (replace, split, join)

-- TODO
-- parsing:
--  > DIRECTORS CUT, ALTERNATE ENDING
--  > Movie Name [1080p]
-- tests

type Title = String
type Year  = Int

data Movie = Movie Title (Maybe Year)
  deriving (Eq, Show)

movie :: GenParser Char st Movie
movie = Movie <$> title <*> try (optionMaybe year)

title :: GenParser Char st Title
title = noDots . trim <$> manyTill anyChar (try . lookAhead $ year <|> (eof >> return 0))
  where noDots = replace "." " " . replace " " " "  -- FIXME: not if movie name actually has dots

year :: GenParser Char st Year
year =
  read <$> between o c p
  where o = oneOf "[(. "
        c = oneOf ")]. "
        p = replicateM 4 digit

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main' :: String -> IO ()
main' s = p . parse movie "(unknown)" $ s
  where p (Left e)  = hPutStrLn stderr $ "ERROR: " ++ show e ++ " during: " ++ s
        p (Right m) = print m

main :: IO ()
main = do
  contents <- getContents
  mapM_ main' (lines contents)
