module Util where -- Export everything

clampInt :: Int -> Int -> Int -> Int
clampInt lo hi x
  | x < lo = lo
  | x > hi = hi
  | otherwise = x

addIfPositive :: Int -> Int -> Int
addIfPositive val delta = if val <= 0 then val else val + delta
