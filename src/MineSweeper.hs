module MineSweeper where
import Data.Array.Repa

-- *...
-- ..*.
-- ....

-- *211
-- 12*1
-- 0111

empty = '-'
mine = '*'
--inputs = repeat 0
inputs = "*.....*....."

board :: Int -> Int -> Array U DIM2 Char
board x y = fromListUnboxed (Z :. x :. y) $ take (x*y) inputs

game = board 4 3

get :: Int -> Int -> Char
get x y | x < 0 = empty
        | y < 0 = empty
        | x >= maxX = empty
        | y >= maxY = empty
        | otherwise = game ! (Z :. x :. y)
        where (Z :. maxY :. maxX) = extent game

neighbors :: Int -> Int -> [Char]
neighbors x y = [ get i j | i <- [x-1..x+1], j <-[y-1..y+1], i/=x || j/=y ]

mines :: Int -> Int -> Int
mines x y = length . filter (== mine) $ neighbors x y




