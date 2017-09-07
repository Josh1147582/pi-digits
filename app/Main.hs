module Main where
import Numeric (showHex, showIntAtBase)
import Data.List (isInfixOf, genericIndex)
import Data.List.Split(splitOn)
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)
import Control.Parallel

import Options.Applicative
import Data.Semigroup ((<>))

-- Get the hex digit of Pi at place n.
hexPi :: Integer -> Integer
hexPi n =
  let
    summation = let
      sone = (4 * sumPi n 1)
      sfour = (2 * sumPi n 4)
      sfive = (sumPi n 5)
      ssix = (sumPi n 6)
      in
      sone `par` sfour `par` sfive `par` ssix `par` sone - sfour - sfive - ssix

    skimmedSum = summation - (fromIntegral (floor summation :: Integer)) -- Take only the decimal portion
  in
    floor (16 * skimmedSum) :: Integer


-- Calculate the summation.
-- 5000 is used in place of Infinity. This value drops off quickly, so no need to go too far.
sumPi :: Integer -> Integer -> Double
sumPi n x =
  let
    summation1 = sum [(fromIntegral ((16^(n-k) `mod` ((8*k)+x)))) / (fromIntegral ((8*k)+x)) | k <- [0..n]]
    summation2 = sum [16^^(n-k) / (fromIntegral ((8*k)+x)) | k <- [(n+1)..5000]]
  in
    summation1 + summation2


-- Get a range of digits.
rangePi :: (Integer -> String) -> String -> Maybe Integer -> Maybe Integer -> String
rangePi printFun delim (Just low) (Just high) =
  if low >= high then
    "Error: Please give a proper range."
  else
    init $ foldr (++) [] $ separate (map printFun (drop (fromIntegral low) . take (fromIntegral high) $ hexDigits)) delim
    -- foldl (.) id (map (showString . printFun) (drop (fromIntegral low) . take (fromIntegral high) $ hexDigits)) ""  -- Alternative implementation: not sure about speed

rangePi _ _ _ _ = printErr


-- Separate a list with some delimiter.
separate :: [a] -> a -> [a]
separate (x:xs) delim = x : delim : separate xs delim
separate [] _ = []


-- Check if response is valid, then call the appropriate function.
parseIndex :: (Integer -> String) -> String -> String -> String
parseIndex printFun delim response =
  if (isInfixOf ".." response) then
    let
      range = splitOn ".." response
      low = readMaybe $ range !! 0 :: Maybe Integer
      high = readMaybe $ range !! 1 :: Maybe Integer
    in
      rangePi printFun delim low high
    else do
      case (readMaybe response :: Maybe Integer) of
        Nothing -> printErr
        Just x -> printFun $ hexDigits `genericIndex` x


-- Continuously prompt for input.
prompt :: (Integer -> String) -> String -> IO ()
prompt printFun delim = do
  putStr ">> "
  hFlush stdout
  response <- getLine
  putStrLn $ parseIndex printFun delim response
  prompt printFun delim


-- The list of answers.
hexDigits :: [Integer]
hexDigits = [hexPi x | x <- [0..]]


-- Complain about argument type.
printErr :: String
printErr = "Error: Please give an Integer (ex: 3) or a range (ex: 3..5)."


main :: IO ()
main = argHandle =<< execParser opts
  where
    opts = info (arguments <**> helper)
      ( fullDesc
     <> progDesc "Generate hexadecimal Pi digits.")


-- Arguments to parse.
data Arguments = Arguments
  { eval  :: Maybe String
  , print :: Maybe PrintFunc
  , delimiter :: String}

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
argHandle (Arguments toEval outputType delim) = do
  let
    printFunIO =
      case outputType of
        Just DecPrint -> do
          putStrLn "Outputting in decimal."
          return (\n -> show n ++ "")
        Just BinPrint -> do
          putStrLn "Outputting in binary."
          return (\n -> showIntAtBase 2 (\x -> show x !! 0) n "")
        _ -> do
          putStrLn "Outputting in hex."
          return (\n -> showHex n "")
    in do
    printFun <- printFunIO
    case toEval of
      (Just s) -> putStrLn $ parseIndex printFun delim s
      _ -> do
        putStrLn "Enter a digit or range (Ctrl-C to exit)."
        prompt printFun delim
