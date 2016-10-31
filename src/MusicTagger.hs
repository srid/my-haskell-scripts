#!/usr/bin/env stack
-- stack --resolver lts-7.5 --install-ghc runghc

-- A script to set ID3 tag based on the directory structure
-- (/$artist/$album?/$track-???.mp3/) of the mp3 files.
-- It outputs the CLI that invokes the id3v2 program.

-- XXX: There are hardcoded paths in this script.

module MusicTagger where

import           Data.Char                     (toLower, toUpper)
import           Data.Maybe                    (fromMaybe)
import           System.Exit                   (exitFailure)
import           System.IO

import           Data.List.Utils               (join, replace)
import           Text.ParserCombinators.Parsec

type Genre  = Int
type Artist = String
type Album  = String
type Track  = String

data Song = Song Genre Artist (Maybe Album) Track
  deriving (Eq, Show)

filePath :: GenParser Char st Song
filePath = Song <$> (pathPrefix *> genre)
                <*> artist
                <*> album
                <*> track

pathPrefix :: GenParser Char st String
pathPrefix =
  string "/Users/Shared/TagFixed/"

genre :: GenParser Char st Genre
genre =
  try a <|> b
  where
    a = string "dl.tamildada.com/"     >> return 48
    b = string "dl.hindugodsongs.com/" >> return 45

pathComponent :: GenParser Char st String
pathComponent =
  many1 (noneOf "/")

artist :: GenParser Char st Artist
artist = pathComponent

album :: GenParser Char st (Maybe Album)
album =
  optionMaybe (try p)
    where p     = between slash slash pathComponent
          slash = char '/'

track :: GenParser Char st Track
track = many anyChar

cleanup :: Song -> Song
cleanup (Song g r a t) =
  Song g
       (fixArtistCase . c  $  r)
       (titleCase     . c <$> a)
       (titleCase     . c  $  t)
  where
    c =
      replace "_" " " .
      replace "-" " " .
      scrub "/" .  -- `album` leaves a trailing slash for Nothing result
      scrub ".mp3" .
      scrub "[TamilDada.Com]" .
      scrub "-[TamilDada.Com]" .
      scrub "-[TamilDada.com]" .
      scrub "-TamilDada.Com" .
      scrub "[-TamilDada.Com]" .
      scrub "-[TamilMovieSongs.Net]" .
      scrub "[TamilMovieSongs.Net]" .
      scrub "{TamilMovieSongs.Net}"
    scrub w = replace w ""

fixArtistCase :: String -> String
fixArtistCase = f . words
  where f [abbr] = if abbr == map toUpper abbr
                   then abbr
                   else titleCase abbr
        f xs     = titleCase (unwords xs)

titleCase :: String -> String
titleCase = unwords .  map f . words
  where f :: String -> String
        f []     = []
        f (x:xs) = toUpper x : map toLower xs

id3v2CLI :: [String] -> String
id3v2CLI = join " " . (:) "id3v2"

tagSetCLI :: String -> Song -> String
tagSetCLI path (Song g r a t) =
  join "\n" [ id3v2CLI [ "-D"]
            , id3v2CLI [ "--artist" , show r
                       , "--album"  , show a'
                       , "--song"   , show t
                       , "--genre"  , show g
                       , show path]]
  where
    a' = fromMaybe (r ++ " Songs") a

main' :: String -> IO ()
main' line =
  case parse filePath "(unknown)" line of
    Left e  -> do hPutStrLn stderr $ "ERROR: " ++ show e
                  exitFailure
    Right s -> (putStrLn . tagSetCLI line . cleanup) s

main :: IO ()
main = do
  contents <- getContents
  mapM_ main' (lines contents)
