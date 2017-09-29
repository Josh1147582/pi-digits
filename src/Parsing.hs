-- Do IO with user input.
module Parsing (argHandle, arguments) where

import Numeric (showHex, showIntAtBase)
import Data.List (isInfixOf, intercalate, genericTake, genericDrop)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import System.IO (stdout, hFlush, hSetBuffering, BufferMode(NoBuffering))

import Logic (hexDigits)

import Options.Applicative
import Data.Semigroup ((<>))


-- Continuously prompt for input.
prompt :: (Integer -> String) -> String -> IO ()
prompt printFun delim = do
  putStr ">> "
  hFlush stdout  -- In case buffering is enabled.
  response <- getLine
  putStrLn $ parseIndex printFun delim response
  prompt printFun delim


-- Check if response is valid, then call the appropriate function.
parseIndex :: (Integer -> String) -> String -> String -> String
parseIndex printFun delim response =
  let
    digits =
      if (isInfixOf ".." response) then
        let
          range = splitOn ".." response
          jLow = readMaybe $ range !! 0 :: Maybe Integer
          jHigh = readMaybe $ range !! 1 :: Maybe Integer
        in
          case (jLow, jHigh) of
            (Just low, Just high) -> getDigitsFrom low high
            _ -> []
      else
        case (readMaybe response :: Maybe Integer) of
          Just n -> getDigitsFrom n (n+1)
          Nothing -> []
  in
    case digits of
      [] -> printErr
      _ -> intercalate delim (map printFun digits)


-- Pull digits in a range.
getDigitsFrom :: Integer -> Integer -> [Integer]
getDigitsFrom low high 
  | low < 0 = []
  | high < 0 = []
  | otherwise = genericDrop low . genericTake high $ hexDigits


-- Complain about argument type.
printErr :: String
printErr = "Error: Please give a positive Integer (ex: 3) or a valid range of positive Integers (ex: 3..5)."


-- Arguments to parse.
data Arguments = Arguments
  { eval  :: Maybe String
  , print :: Maybe PrintFunc
  , delimiter :: String
  , buffer :: Bool}

arguments :: Parser Arguments
arguments = Arguments
      <$> optional ( argument str (
                       help "Evaluate an index/range, and exit."
                       <> (metavar "eval")))
      <*> printFunc
      <*> strOption ( long "delimiter"
                        <> metavar "delim"
                        <> value ""
                        <> help "Delimiter to separate printed values.")
      <*> switch (long "buffer"
               <> help "Don't print output as it's calculated.")



-- Decide which printing function to use.
data PrintFunc = DecPrint | BinPrint

printFunc :: Parser (Maybe PrintFunc)
printFunc = optional (decPrint <|> binPrint)

decPrint :: Parser PrintFunc
decPrint = flag' DecPrint ( long "decimal"
                            <> short 'd'
                            <> help "Output in decimal.")

binPrint :: Parser PrintFunc
binPrint = flag' BinPrint ( long "binary"
                            <> short 'b'
                            <> help "Output in binary.")


-- Handle args, either prompt or eval & quit.
argHandle :: Arguments -> IO ()
argHandle (Arguments toEval outputType delim buff) = do
  let
    printFunIO =
      case outputType of
        Just DecPrint -> do
          putStrLn "Outputting in decimal."
          return (\n -> show n)
        Just BinPrint -> do
          putStrLn "Outputting in binary."
          return (\n -> showIntAtBase 2 (\x -> show x !! 0) n "")
        _ -> do
          putStrLn "Outputting in hex."
          return (\n -> showHex n "")
    in do
    printFun <- printFunIO
    if buff then
      return ()
    else
      hSetBuffering stdout NoBuffering
    case toEval of
      Just s -> putStrLn $ parseIndex printFun delim s
      _ -> do
        putStrLn "Enter a digit or range (Ctrl-C to exit)."
        prompt printFun delim
