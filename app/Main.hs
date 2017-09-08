module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Parsing (argHandle, arguments)

main :: IO ()
main = argHandle =<< execParser opts
  where
    opts = info (arguments <**> helper)
      ( fullDesc
     <> progDesc "Generate hexadecimal Pi digits.")


