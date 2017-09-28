-- Calculate and store values of Pi.
module Logic (hexDigits) where

import Control.Parallel (par)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)


-- Get the hex digit of Pi at place n.
hexPi :: Integer -> Integer
hexPi n =
  let
    summation = let
      sOne = (4 * sumPi n 1)
      sFour = (2 * sumPi n 4)
      sFive = (sumPi n 5)
      sSix = (sumPi n 6)
      in
      sOne `par` sFour `par` sFive `par` sSix `par` sOne - sFour - sFive - sSix

    skimmedSum = summation - (fromIntegral (floor summation :: Integer)) -- Take only the decimal portion
  in
    floor (16 * skimmedSum) :: Integer


-- Calculate the summation.
-- 5000 is used in place of Infinity. This value drops off quickly, so no need to go too far.
sumPi :: Integer -> Integer -> Double
sumPi n x =
  let
    summation1 = sum [(fromIntegral (fastModExp 16 (n-k) ((8*k)+x))) / (fromIntegral ((8*k)+x)) | k <- [0..n]]
    summation2 = sum [16^^(n-k) / (fromIntegral ((8*k)+x)) | k <- [(n+1)..5000]]
  in
    summation1 + summation2


-- The list of answers.
hexDigits :: [Integer]
hexDigits = [hexPi x | x <- [0..]]


-- Calculate a^b mod c.
fastModExp :: Integer -> Integer -> Integer -> Integer
fastModExp a b c =
  let
    -- Represent b as a binary string, and reverse it.
    -- This lets index n indicate 2^n.
    revBinaryB = reverse (showIntAtBase 2 intToDigit b "")

    powersOfA = [a^(2^n) `mod` c | n <- [0..]]

    -- Take only binary powers of a that comprise b.
    bPowersOfA = map (\(_, n) -> n) $ filter (\(char, _) -> char == '1') $ zip revBinaryB powersOfA
  in
    foldr (\m n -> (m * n) `mod` c) 1 bPowersOfA
