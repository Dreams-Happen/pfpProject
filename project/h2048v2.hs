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

getChildren :: Grid -> [Grid]
getChildren grid = filter (\x -> x /= grid) [move d grid | d <- directions]
    where directions = [Left, Right, Up, Down]


printGrid :: Grid -> IO ()
printGrid grid = do
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
    let m = Left --getAIMove grid
    let new_grid = move m grid
    return new_grid

sumOfTiles :: Grid -> Integer
sumOfTiles grid = toInteger $ sum $ map sum grid


weightMatrix grid = sumOfTiles $ zipWith (zipWith (*)) matrix grid
    --where matrix = [[1073741824, 268435456, 67108864, 16777216],[65536,262144,1048576,4194304],[16384,4096,1024,256],[1,4,16,64]]   
    --where matrix = [[1073741824, 268435456, 67108864, 16777216],[4194304,1048576,262144,65536],[16384,4096,1024,256],[64,16,4,1]]
    --where matrix = [[7,6,5,4],[6,5,4,3],[5,4,3,2],[4,3,2,1]] 
    where matrix = if maxTile grid <= 512 then [[21,8,3,3],[9,5,2],[4,3]] else  [[19,9,5,3],[8,4,2],[3]]
    --where matrix = [[26000,,22,20],[12,14,16,18],[10,8,6,4],[1,2,3,4]]

monotonicity [] _ = 0
monotonicity (x:xs) currentValue = monotonicityHelper x currentValue + monotonicity xs currentValue

monotonicityHelper [] _ = 0
monotonicityHelper (x:xs) currentValue 
                | x < currentValue = 1 + monotonicityHelper xs x 
                | otherwise = monotonicityHelper xs x 


smoothness [] = 0
smoothness (x:xs) = smoothnessHelper x + smoothness xs 

smoothnessHelper [] = 0
smoothnessHelper (a:b:c:d:xs) = fromIntegral $ ((abs (a - b)) + (abs (b - c)) + (abs (c - d))) * 5
                


availableCells :: Grid -> Integer
availableCells grid = toInteger $ sum $ map zeros grid
    where zeros l = length $ filter (\x -> x == 0) l

weWon grid 
    | maxTile grid == 2048 = 999999
    | otherwise = 0


utility :: Grid -> Integer
utility grid = weightMatrix grid + (100 * availableCells grid) + (15 * monotonicity grid 9999) + (15 * monotonicity (transpose grid) 9999) - (smoothness grid) - (smoothness (transpose grid)) + weWon grid
--utility grid = fromIntegral $ (3 * monotonicity grid 9999) + (3 * monotonicity (transpose grid) 9999) - (smoothness grid) - (smoothness (transpose grid)) + (maxTile grid) + (fromIntegral $ 3 * availableCells grid) + (fromIntegral $ sumOfTiles grid)


maximize grid a b maxDepth
  | maxDepth == 0 || not (isMoveLeft grid) = (grid, utility grid)
  | otherwise = maxHelper Children grid a b (-999999999999) maxDepth
    where children = if length (getNoRight grid) > 0 then getNoRight grid else getChildren grid

maxHelper [] maxChild a b maxUtility maxDepth = (maxChild, maxUtility)
maxHelper (c:children) maxChild a b maxUtility maxDepth
  | utility > maxUtility = maxHelper (c:children) c a b utility maxDepth
  | maxUtility >= b = maxHelper children maxChild a b maxUtility maxDepth
  | maxUtility > a = maxHelper (c:children) maxChild maxUtility b maxUtility maxDepth
  | otherwise = maxHelper children maxChild a b maxUtility maxDepth
    where utility = chance c a b (maxDepth - 1)


chance grid a b maxDepth  
  | maxDepth == 0 = utility grid
  | otherwise = toInteger (round ((9/10) * (realToFrac (minimize grid a b 2 (maxDepth - 1))))) + toInteger (round ((1/10) * (realToFrac (minimize grid a b 4 (maxDepth - 1)))))


minimize grid a b tileVal maxDepth
  | maxDepth == 0 || availableCells grid == 0 = utility grid
  | otherwise = minHelper grid (getZeroes grid) tileVal a b 999999999999 maxDepth

minHelper grid [] tVal a b minUtility maxDepth = minUtility
minHelper grid (c:cells) tVal a b minUtility maxDepth
  | utility < minUtility = minHelper grid (c:cells) tVal a b utility maxDepth
  | minUtility <= a = minHelper grid cells tVal a b minUtility maxDepth
  | minUtility < b = minHelper grid (c:cells) tVal a minUtility minUtility maxDepth
  | otherwise = minHelper grid cells tVal a b minUtility maxDepth
    where (_, utility) = maximize (setSquare grid c tVal) a b (maxDepth - 1)


maxTile :: Grid -> Int
maxTile b = maximum $ map maximum b

eval :: Grid -> Int
eval b
 | (maxTile b) <= 512 = sum $ map (\(x,y) -> sum $ zipWith (*) x y) c
 | otherwise = sum $ map (\(x,y) -> sum $ zipWith (*) x y) d
 where
 c = zip [[21,8,3,3],[9,5,2],[4,3]] b
 d = zip [[19,9,5,3],[8,4,2],[3]] b


gameLoop :: Grid -> Int -> IO ()
gameLoop grid moves
    | isMoveLeft grid = do
        printGrid grid
        if check2048 grid
        then putStrLn "You won!"
        else do 
                let (newGrid, _) = maximize grid (-999999999999) 999999999999 4
                if grid /= newGrid
                then do new <- addTile newGrid
                        gameLoop new (moves + 1)
                else gameLoop grid moves
    | otherwise = do
        printGrid grid
        putStrLn "Game over"
        putStrLn $ "number of moves: " ++ show moves

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    grid <- start
    gameLoop grid 0
