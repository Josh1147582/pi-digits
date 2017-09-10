-- Calculate and store values of Pi.
module Logic (hexDigits) where

import Control.Parallel (par)


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
    summation1 = sum [(fromIntegral (fastMod 16 (n-k) ((8*k)+x))) / (fromIntegral ((8*k)+x)) | k <- [0..n]]
    summation2 = sum [16^^(n-k) / (fromIntegral ((8*k)+x)) | k <- [(n+1)..5000]]
  in
    summation1 + summation2


-- The list of answers.
hexDigits :: [Integer]
hexDigits = [hexPi x | x <- [0..]]


fastMod :: Integer -> Integer -> Integer -> Integer
fastMod b n k =
  let
    t = largestT 0 n
  in
    a b (fromIntegral n) k 1 t

-- Get the largest t such that t^2 <= n
largestT :: Integer -> Integer -> Double
largestT t n
  | (2 ^ (t + 1)) <= n = largestT (t + 1) n
  | otherwise = 2^^t

a :: Integer -> Double -> Integer -> Integer -> Double -> Integer
a b n k r t =
  if n >= t then
    let
      r' = (b * r) `mod` k
      n' = n - t
      t' = t / 2
    in
      if t' >= 1 then
          a b n' k ((r' ^ 2) `mod` k) t'
      else
        r'
  else
    let t' = t / 2 in
      if t' >= 1 then
          a b n k ((r ^ 2) `mod` k) t'
      else
        r
