module MineSweeper where
import Data.Array.Repa hiding ((++))

data Board = Board { width :: Int
                   , height :: Int
                   , input :: [Char]
                   }
instance Show Board where
        show (Board w h input) = show input
        
data Cell = Mine | NotCounted | MineCount Int | NoMines deriving (Read)
        
instance Show Cell where
        show Mine = "*"
        show (MineCount a) = show a
        show NoMines = "-"
        show NotCounted = "."

readCell:: Char -> Cell
readCell '*' = Mine
readCell '.' = NotCounted

-- *...
-- ..*.
-- ....

-- *211
-- 12*1
-- 0111

empty = '-'
mine = '*'
--inputs = repeat 0
mines = Board 4 3 "*.....*....."

board :: Board -> Array U DIM2 Char
board (Board x y inputs) = fromListUnboxed (Z :. y :. x) $ take (x*y) inputs

get :: Board -> Int -> Int -> Char
get b x y 
        | x < 0 = empty
        | y < 0 = empty
        | x >= w = empty
        | y >= h = empty
        | otherwise = (board b) ! (Z :. y :. x)
        where (Board w h _) = b

neighbors :: Board -> Int -> Int -> [Char]
neighbors b x y = [ get b i j | i <- [x-1..x+1], j <-[y-1..y+1], i/=x || j/=y ]

showMine :: Board -> Int -> Int -> Char
showMine b x y 
        | get b x y == mine = mine
        | otherwise = head . show . length . filter (== mine) $ neighbors b x y

countMines :: Board -> [Char]
countMines b = [showMine b x y | y <-[0..h-1], x <- [0..w-1] ]
        where (Board w h _) = b

openAll :: Board -> Board
openAll b = Board w h (countMines b)
        where (Board w h _) = b

open :: Board -> Int -> Int -> Board
open b x y = Board w h (openMine b x y)
        where (Board w h _) = b
        
openMine :: Board -> Int -> Int -> [Char]
openMine b x1 y1 = [if x==x1 && y==y1 then showMine b x y else (last (take (x+y*w+1) m)) | y <-[0..h-1], x <- [0..w-1] ]
        where (Board w h m) = b
