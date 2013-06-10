module MineSweeper where
import Data.Array.Repa


--inputs = repeat 0
inputs = [1..]

board :: Int -> Int -> Array U DIM2 Int
board x y = fromListUnboxed (Z :. x :. y) $ take (x*y) inputs

game = board 3 3

get :: Int -> Int -> Int
get x y | x < 0 = 0
        | y < 0 = 0
        | x >= maxX = 0
        | y >= maxY = 0
        | otherwise = game ! (Z :. x :. y)
        where (Z :. maxY :. maxX) = extent game

neighbors :: Int -> Int -> [Int]
neighbors x y = [ get i j | i <- [x-1..x+1], j <-[y-1..y+1], i/=x || j/=y ]

mines = length . filter (/= 0)