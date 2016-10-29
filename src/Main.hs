module Main where

import System.IO
import Control.Monad

dataFile :: String
dataFile = "movies-list.txt"

main :: IO ()
main = do
  handle <- openFile dataFile ReadMode
  contents <- hGetContents handle
  -- let output = parseMovie <$> lines contents
  -- print output
  hClose handle
