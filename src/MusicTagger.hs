#!/usr/bin/env stack
-- stack --resolver lts-7.5 --install-ghc runghc

-- A script to set ID3 tag based on the directory structure
-- (/$artist/$album?/$track-???.mp3/) of the mp3 files.
-- It outputs the CLI that invokes the id3v2 program.

-- XXX: There are hardcoded paths in this script.

module MusicTagger where

import System.IO
import System.Exit (exitFailure)
import Data.Char (toUpper, toLower)
import Data.Maybe (fromMaybe)

import Text.ParserCombinators.Parsec
import Data.List.Utils (replace, split, join)

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
cleanup (Song genre artist album track) =
  Song genre
       (fixArtistCase . c  $  artist)
       (titleCase     . c <$> album)
       (titleCase     . c  $  track)
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
tagSetCLI path (Song genre artist albumMaybe track) =
  join "\n" [ id3v2CLI [ "-D"]
            , id3v2CLI [ "--artist" , show artist
                       , "--album"  , show album
                       , "--song"   , show track
                       , "--genre"  , show genre
                       , show path]]
  where
    album = fromMaybe (artist ++ " Songs") albumMaybe

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
