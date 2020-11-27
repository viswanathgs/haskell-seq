module Utils where

type Matrix a = [[a]]

(!) :: Matrix a -> (Int, Int) -> a
a ! (i, j) = a !! i !! j
infix 6 !

rows :: Matrix a -> Int
rows = length

cols :: Matrix a -> Int
cols [] = 0
cols a = length . head $ a
