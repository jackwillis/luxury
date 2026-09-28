module Main where

import System.IO (isEOF)

main :: IO ()
main = do
  putStrLn "luxury-scheme"
  loop

loop :: IO ()
loop = do
  eof <- isEOF
  if eof
    then pure ()
    else do
      line <- getLine
      putStrLn ("you typed: " <> line)
      loop
