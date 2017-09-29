-- Calculate and store values of Pi.
module Logic (hexDigits) where

import Data.Bits (testBit, shiftR)
import Control.Parallel.Strategies


-- Get the hex digit of Pi at place n.
hexPi :: Integer -> Integer
hexPi n =
  let
    summation = (4 * sumPi n 1) - (2 * sumPi n 4) - (sumPi n 5) - (sumPi n 6)
  in
    floor (16 * skim summation) :: Integer


-- Calculate the summation.
-- n+5000 is used in place of Infinity. This value drops off quickly, so no need to go too far.
sumPi :: Integer -> Integer -> Double
sumPi n x =
  let
    summation1 = sum ([skim $ (fromIntegral (modExp 16 (n-k) ((8*k)+x))) / (fromIntegral ((8*k)+x)) | k <- [0..n]] `using` parList rseq)
    summation2 = sum ([skim $ (16^^(n-k)) / (fromIntegral ((8*k)+x)) | k <- [(n+1)..(n+5000)]] `using` parList rseq)
  in
    summation1 + summation2


-- Take only the decimal portion of a number.
skim :: Double -> Double
skim n
  | n == (read "Infinity" :: Double) = 0  -- Without this check, skim returns NaN
  | otherwise = n - fromIntegral (floor n :: Integer)


-- The list of answers.
hexDigits :: [Integer]
hexDigits = [hexPi x | x <- [0..]]


modExp :: Integer -> Integer -> Integer -> Integer
modExp _ 0 _ = 1
modExp a b c =
  let
    n = if testBit b 0 then a `mod` c else 1
  in
    n * modExp ((a^2) `mod` c) (shiftR b 1) c `mod` c
