module Main where
import Numeric (showHex)
import Data.List (isInfixOf, genericIndex)
import Data.List.Split(splitOn)
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)

hexPi :: Integer -> Char
hexPi n =
  let
    summation = (4 * sumPi n 1) - (2 * sumPi n 4) - (sumPi n 5) - (sumPi n 6)
    skimmedSum = summation - (fromIntegral (floor summation :: Integer))
  in
    showHex (floor (16 * skimmedSum) :: Integer) "" !! 0

sumPi :: Integer -> Integer -> Double
sumPi n x =
  let
    summation1 = [(fromIntegral ((16^(n-k) `mod` ((8*k)+x)))) / (fromIntegral ((8*k)+x)) | k <- [0..n]]
    -- 5000 is used in place of Infinity. This value drops off quickly, so no need to go too far.
    summation2 = [16^^(n-k) / (fromIntegral ((8*k)+x)) | k <- [(n+1)..5000]]
  in
    sum $ summation1 ++ summation2

rangePi :: Maybe Integer -> Maybe Integer -> IO ()
rangePi num1 num2 = do
  case (num1, num2) of
    (Just one, Just two) ->
      if one >= two then
        putStrLn "Please give a proper range."
      else
        putStrLn $ foldr (:) [] (drop (fromIntegral one) . take (fromIntegral two) $ hexDigits)
    (_,_) -> putStrLn "Error: Please give an Integer (ex: 3) or a range (ex: 3..5)."

prompt :: IO ()
prompt = do
  putStr ":: "
  hFlush stdout
  response <- getLine
  -- Check if response is range, or single digit.
  if (isInfixOf ".." response) then
    let
      range = splitOn ".." response
    in
      rangePi (readMaybe $ range !! 0 :: Maybe Integer) (readMaybe $ range !! 1 :: Maybe Integer)
    else do
      case (readMaybe response :: Maybe Integer) of
        Nothing -> putStrLn "Error: Please give an Integer (ex: 3) or a range (ex: 3..5)."
        Just x -> putStrLn $ hexDigits `genericIndex` x :[]
  prompt

-- Establish the list of answers
hexDigits :: [Char]
hexDigits = [hexPi x | x <- [0..]]

main :: IO ()
main = do
  putStrLn "Enter a digit or range (Ctrl-C to exit)"
  prompt 
