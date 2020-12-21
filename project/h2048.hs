{-

h2048
A Haskell implementation of 2048.

Gregor Ulm

last update:
2014-06-18

Please consult the file README for further information
on this program.

-}

import Prelude hiding (Left, Right)
import Data.Char (toLower)
import Data.List
import System.IO
import System.Random
import Text.Printf

data Move = Up | Down | Left | Right
type Grid = [[Int]]

start :: IO Grid
start = do grid'  <- addTile $ replicate 4 [0, 0, 0, 0]
           addTile grid'

merge :: [Int] -> [Int]
merge xs = merged ++ padding
    where padding = replicate (length xs - length merged) 0
          merged  = combine $ filter (/= 0) xs
          combine (x:y:xs) | x == y = x * 2 : combine xs
                           | otherwise = x  : combine (y:xs)
          combine x = x

move :: Move -> Grid -> Grid
move Left  = map merge
move Right = map (reverse . merge . reverse)
move Up    = transpose . move Left  . transpose
move Down  = transpose . move Right . transpose

getZeroes :: Grid -> [(Int, Int)]
getZeroes grid = filter (\(row, col) -> (grid!!row)!!col == 0) coordinates
    where singleRow n = zip (replicate 4 n) [0..3]
          coordinates = concatMap singleRow [0..3]

setSquare :: Grid -> (Int, Int) -> Int -> Grid
setSquare grid (row, col) val = pre ++ [mid] ++ post
    where pre  = take row grid
          mid  = take col (grid!!row) ++ [val] ++ drop (col + 1) (grid!!row)
          post = drop (row + 1) grid

isMoveLeft :: Grid -> Bool
isMoveLeft grid = sum allChoices > 0
    where allChoices = map (length . getZeroes . flip move grid) directions
          directions = [Left, Right, Up, Down]

getChildren :: Grid -> [(Grid,Move)]
getChildren grid = filter (\x -> fst x /= grid) [(move d grid, d) | d <- directions]
    where directions = [Left, Right, Up, Down]


printGrid :: Grid -> IO ()
printGrid grid = do
    --putStr "\ESC[2J\ESC[2J\n" -- clears the screen
    putStrLn ""
    mapM_ (putStrLn . showRow) grid

showRow :: [Int] -> String
showRow = concatMap (printf "%5d")

moves :: [(Char, Move)]
moves = keys "wasd" ++ keys "chtn"
    where keys chars = zip chars [Up, Left, Down, Right]

captureMove :: IO Move
captureMove = do
    inp <- getChar
    case lookup (toLower inp) moves of
        Just x  -> return x
        Nothing -> do putStrLn "Use WASD or CHTN as input"
                      captureMove

check2048 :: Grid -> Bool
check2048 grid = [] /= filter (== 2048) (concat grid)
                
addTile :: Grid -> IO Grid
addTile grid = do
    let candidates = getZeroes grid
    pick <- choose candidates
    val  <- choose [2,2,2,2,2,2,2,2,2,4]
    let new_grid = setSquare grid pick val
    return new_grid

choose :: [a] -> IO a
choose xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

newGrid :: Grid -> IO Grid
newGrid grid = do
    let m = getAIMove grid
    let new_grid = move m grid
    return new_grid

sumOfTiles :: Grid -> Int
sumOfTiles grid = sum $ map sum grid

-- utility :: Grid -> Int
-- utility grid = weightMatrix grid + 1024 * (availableCells grid)

weightMatrix grid = sumOfTiles $ zipWith (zipWith (*)) matrix grid
    where matrix = [[1073741824, 268435456, 67108864, 16777216],[65536,262144,1048576,4194304],[16384,4096,1024,256],[1,4,16,64]]

availableCells grid = sum $ map zeros grid
    where zeros l = length $ filter (\x -> x == 0) l

getAIMove :: Grid -> Move 
getAIMove grid = backTrackMove grid (getChildren grid) (optimalMove grid (-99999) [minimizer (fst possibleGrid) (fst possibleGrid) 1 | possibleGrid <- getChildren grid])

minimizer :: Grid -> Grid -> Int -> (Grid, Grid, Int)
minimizer grid originalGrid depth 
            | depth > 4 || length possibleMoves == 0 = (grid, originalGrid, utility grid)
            | otherwise = minimizerHelper possibleMoves grid originalGrid 99999 depth
              where possibleMoves = moves2 ++ moves4
                    moves2 = [fst x | x <- random2]
                    moves4 = [fst x | x <- random4]
                    random2 = randomGrid grid 2
                    random4 = randomGrid grid 4

minimizerHelper :: [Grid] -> Grid -> Grid -> Int -> Int -> (Grid, Grid, Int)
minimizerHelper [] grid originalGrid minimumUtility _ = (grid, originalGrid, minimumUtility)
minimizerHelper (x:xs) grid originalGrid minimumUtility depth 
        | currentUtility < minimumUtility = minimizerHelper xs x originalGrid currentUtility depth
        | otherwise = minimizerHelper xs grid originalGrid minimumUtility depth 
            where currentUtility = snd (maximizer x (depth + 1))
    
maximizer :: Grid -> Int -> (Grid, Int) 
maximizer grid depth 
    | depth > 4 = (grid, utility grid)
    | length (getChildren grid) == 0 = (grid, 0)
    | otherwise = optimalMove grid (-99999) [minimizer (fst possibleGrid) (fst possibleGrid) (depth+1) | possibleGrid <- getChildren grid] 

optimalMove :: Grid -> Int -> [(Grid, Grid, Int)] -> (Grid, Int)
optimalMove grid maximumUtility [] = (grid, maximumUtility) 
optimalMove grid maximumUtility ((_,currentGrid,utility):xs)
        | utility > maximumUtility = optimalMove currentGrid utility xs 
        | otherwise = optimalMove grid maximumUtility xs 


backTrackMove :: Grid -> [(Grid,Move)] -> (Grid, Int) -> Move
backTrackMove grid [] optimalGridResult = randomMove grid 
backTrackMove grid listOfPossibilities optimalGridResult 
        | fst (head listOfPossibilities) == fst optimalGridResult = snd (head listOfPossibilities)
        | otherwise = backTrackMove grid (tail listOfPossibilities) optimalGridResult 

insertTile :: (Int, Int) -> Int -> Grid -> Grid
insertTile (rowIndex, columnIndex) value = updateIndex (updateIndex (const value) columnIndex) rowIndex
 where updateIndex fn i list = take i list ++ fn (head $ drop i list) : tail (drop i list)

randomGrid :: Grid -> Int -> [(Grid, Int)]
randomGrid grid insertedValue = sortOn (\(_,d) -> -d) [(insertTile (x,y) insertedValue grid, risk grid x y) | (x,y) <- getZeroes grid]

randomMove :: Grid -> Move 
randomMove grid = snd (head (getChildren grid))

maximumCoordinate :: Grid -> Int
maximumCoordinate grid = maximum $ map maximum grid

utility :: Grid -> Int
utility grid
 | (maximumCoordinate grid) <= 512 = sum $ map (\(x,y) -> sum $ zipWith (*) x y) a
 | otherwise = sum $ map (\(x,y) -> sum $ zipWith (*) x y) b
 where
 a = zip [[30,27,24,21], [13,15,17],[7,5]] grid
 b = zip [[30,27,24,21], [13,15,17],[5]] grid


risk :: Grid -> Int -> Int -> Int
risk grid x y = (riskUp grid x y) + (riskDown grid x y) + (riskLeft grid x y) + (riskRight grid x y)

riskUp :: Grid -> Int -> Int -> Int
riskUp grid x y
 | x > 0 = abs $ ((grid !! x) !! y) - ((grid !! (x-1)) !! y)
 | otherwise = 0

riskDown :: Grid -> Int -> Int -> Int
riskDown grid x y
 | x < 3 = abs $ ((grid !! x) !! y) - ((grid !! (x+1)) !! y)
 | otherwise = 0

riskLeft :: Grid -> Int -> Int -> Int
riskLeft grid x y
 | y > 0 = abs $ ((grid !! x) !! y) - ((grid !! x) !! (y-1))
 | otherwise = 0


riskRight :: Grid -> Int -> Int -> Int
riskRight grid x y
 | y < 3 = abs $ ((grid !! x) !! y) - ((grid !! x) !! (y+1))
 | otherwise = 0


gameLoop :: Grid -> IO ()
gameLoop grid
    | isMoveLeft grid = do
        printGrid grid
        if check2048 grid
        then putStrLn "You won!"
        else do new_grid <- newGrid grid
                if grid /= new_grid
                then do new <- addTile new_grid
                        gameLoop new
                else gameLoop grid
    | otherwise = do
        printGrid grid
        putStrLn "Game over"

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    grid <- start
    gameLoop grid
