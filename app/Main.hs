module Main where
import Numeric (showHex, showIntAtBase)
import Data.List (isInfixOf, genericIndex)
import Data.List.Split(splitOn)
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Control.Parallel

-- Get the hex representation of Pi at place n.
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
    summation1 = [(fromIntegral ((16^(n-k) `mod` ((8*k)+x)))) / (fromIntegral ((8*k)+x)) | k <- [0..n]]
    summation2 = [16^^(n-k) / (fromIntegral ((8*k)+x)) | k <- [(n+1)..5000]]
  in
    sum $ summation1 ++ summation2


-- Get a range of digits.
rangePi :: (Integer -> String) -> Maybe Integer -> Maybe Integer -> String
rangePi printFun (Just low) (Just high) =
  if low >= high then
    "Error: Please give a proper range."
  else
    foldr (++) [] (map printFun (drop (fromIntegral low) . take (fromIntegral high) $ hexDigits))

rangePi _ _ _  = printErr


prompt :: (Integer -> String) -> IO ()
prompt printFun = do
  putStr ":: "
  hFlush stdout
  response <- getLine
  -- Check if response is range, or single digit.
  if (isInfixOf ".." response) then
    let
      range = splitOn ".." response
      low = readMaybe $ range !! 0 :: Maybe Integer
      high = readMaybe $ range !! 1 :: Maybe Integer
    in
      putStrLn $ rangePi printFun low high
    else do
      case (readMaybe response :: Maybe Integer) of
        Nothing -> putStrLn printErr
        Just x -> putStrLn $ printFun $ hexDigits `genericIndex` x
  prompt printFun


-- The list of answers.
hexDigits :: [Integer]
hexDigits = [hexPi x | x <- [0..]]


-- Complain about argument type.
printErr :: String
printErr = "Error: Please give an Integer (ex: 3) or a range (ex: 3..5)."


main :: IO ()
main = do
  -- Get formatter function (hex, binary, or decimal)
  args <- getArgs
  let
    printFunIO =
      case args of
          ["-b"] -> do
            putStrLn "Outputting in binary."
            return (\n -> showIntAtBase 2 (\x -> show x !! 0) n "")
          ["-d"] -> do
            putStrLn "Outputting in decimal."
            return (\n -> show n ++ " ")
          ["-h"] -> do
            putStrLn "Generate hexadecimal Pi digits. Output in hexidemical by default.\n\
                     \\t-b\tOutput in binary.\n\
                     \\t-d\tOutput in decimal.\n\
                     \\t-h\tShow this help message."
            exitWith ExitSuccess
          _ -> do
            putStrLn "Outputting in hex."
            return (\n -> showHex n "")
    in do
      printFun <- printFunIO
      putStrLn "Enter a digit or range (Ctrl-C to exit)."
      prompt printFun
