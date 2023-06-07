import Data.List
import Data.Maybe
import Data.Char
import Control.Monad (guard)

import System.Environment (getArgs)
import System.IO (hGetContents, stdin, openFile, IOMode(ReadMode))


readLinesToList :: FilePath -> IO [String]
readLinesToList filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  return linesList



main :: IO ()
main = do
  args <- getArgs
  linesList <- case args of
    [filePath] -> readLinesToList filePath
    _ -> hGetContents stdin >>= return . lines
  mapM_ putStrLn linesList
  putStr "\nSolucion\n"
  let sudoku_problem = parseLines linesList
  let dp = dpll ( allFilled <&&> noneFilledTwice <&&> rowsComplete
                  <&&> columnsComplete <&&> squaresComplete
                  <&&> (sudoku_problem) <&&> rowsNoRepetition
                  <&&> columnsNoRepetition <&&> squaresNoRepetition)
  putStr (matrixToString( concat [(findMissingN (findTuples (dp) i j)) | i <- [1..9], j <- [1..9]]))


data Var = A | B | C | D | E | F | G | H
 deriving (Eq,Show)

data Literal atom = P atom | N atom
 deriving Eq

instance Show a => Show (Literal a) where
 show (P x) = show x
 show (N x) = "not " ++ show x


data Clause atom = Or [Literal atom]
 deriving Eq

instance Show a => Show (Clause a) where
 show (Or ls) = "(" ++ intercalate " || " (map show ls) ++ ")"


data Form atom = And [Clause atom]
 deriving Eq

instance Show a => Show (Form a) where
 show (And ls) = intercalate " && " (map show ls)

cnf' = And [Or [N A, N B, P C], Or [N A, P D, P F], Or [P A, P B, P E], Or [P A, P B, N C]]


(<<) :: Eq atom => [Clause atom] -> Literal atom -> [Clause atom]
cs << l = [ Or (delete (neg l) ls)
 | Or ls <- cs, not (l `elem` ls) ]
neg :: Literal atom -> Literal atom
neg (P a) = N a
neg (N a) = P a


prioritise :: Form atom -> [Clause atom]
prioritise (And cs) = sortOn (\(Or ls) -> length ls) cs

dpll :: Eq atom => Form atom -> [[Literal atom]]
dpll f =
 case prioritise f of
  [] -> [[]] -- the trivial solution
  Or [] : cs -> [] -- no solution
  Or (l:ls) : cs -> [ l : ls | ls <- dpll (And (cs << l)) ] ++ [ neg l : ls | ls <- dpll (And (Or ls : cs << neg l)) ]


allFilled :: Form (Int,Int,Int)
allFilled = And [ Or [ P (i,j,n) | n <- [1..9] ]
  | i <- [1..9], j <- [1..9] ]

noneFilledTwice :: Form (Int,Int,Int)
noneFilledTwice = And [ Or [ N (i, j, n), N (i, j, n') ]
 | i <- [1..9], j <- [1..9], n <- [1..9], n' <- [1..(n-1)]]

rowsComplete :: Form (Int,Int,Int)
rowsComplete = And [ Or [ P (i, j, n) | j <- [1..9] ]
 | i <- [1..9], n <- [1..9] ]


columnsComplete :: Form (Int, Int, Int)
columnsComplete = And [ Or [ P (i, j, n) | i <- [1..9] ]
 | j <- [1..9], n <- [1..9]]


squaresComplete :: Form (Int, Int, Int)
squaresComplete = And [ Or [ P (i, j, n) | i <- [r..r+2], j <- [c..c+2] ]
                      | r <- [1,4,7], c <- [1,4,7], n <- [1..9] ]

rowsNoRepetition :: Form (Int,Int,Int)
rowsNoRepetition = And [ Or [ N (i, j, n), N (i, j', n) ]
 | i <- [1..9], n <- [1..9], j <- [1..9], j' <- [1..(j-1)] ]


columnsNoRepetition :: Form (Int, Int, Int)
columnsNoRepetition = And [ Or [ N (i, j, n), N (i', j, n) ]
  | i <- [1..9], n <- [1..9], j <- [1..9], i' <- [1..(i-1)] ]

squaresNoRepetition :: Form (Int, Int, Int)
squaresNoRepetition = And [ Or [ N (i, j, n), N (i', j', n) ]
  | i <- [1, 4, 7], j <- [1, 4, 7]
  , i' <- [i..(i+2)], j' <- [j..(j+2)]
  , n <- [1..9]
  , (i, j) /= (i', j')]


(<&&>) :: Form a -> Form a -> Form a
And xs <&&> And ys = And ( xs ++ ys )

sudoku =
 allFilled <&&> noneFilledTwice <&&> rowsComplete
 <&&> columnsComplete <&&> squaresComplete
 <&&> rowsNoRepetition
 <&&> columnsNoRepetition <&&> squaresNoRepetition
 -- <&&> sudokuProblem 

findTuples :: [[Literal (Int, Int, Int)]] -> Int -> Int -> [[Literal (Int, Int, Int)]]
findTuples forms x y = filterTuples forms
  where
    filterTuples :: [[Literal (Int, Int, Int)]] -> [[Literal (Int, Int, Int)]]
    filterTuples [] = []
    filterTuples (clause:rest) = filterLiterals clause : filterTuples rest

    filterLiterals :: [Literal (Int, Int, Int)] -> [Literal (Int, Int, Int)]
    filterLiterals literals = filter isTargetLiteral literals

    isTargetLiteral :: Literal (Int, Int, Int) -> Bool 
    isTargetLiteral (N (a, b, _)) = a == x && b == y
    isTargetLiteral _ = False



findMissingN :: [[Literal (Int, Int, Int)]] -> [(Int, Int, Int)]
findMissingN literals = missingLiteral
  where
    allNs = nub [n | N (x, y, n) <- concat literals]  -- Extract all 'n' values
    missingNs
      | null missingValues = [0]
      | otherwise = missingValues
    missingValues = [n | n <- [1..9], n `notElem` allNs]  -- Find missing 'n' values
    missingLiteral = [(x, y, n) | n <- missingNs, let (x, y, _) = head [(x', y', n') | N (x', y', n') <- concat literals, n' /= n]]

-- Helper function to extract the value from a tuple
getValue :: (Int, Int, Int) -> Int
getValue (_, _, n) = n

-- Helper function to get the maximum column number
maxCol :: [(Int, Int, Int)] -> Int
maxCol = maximum . map (\(_, c, _) -> c)

-- Helper function to create a row of values
getRowValues :: [(Int, Int, Int)] -> Int -> String
getRowValues tuples row = concatMap (\(_, c, n) -> show n ++ " ") sortedTuples
  where
    sortedTuples = sort $ filter (\(r, _, _) -> r == row) tuples

-- Function to convert the list of tuples to a matrix string
matrixToString :: [(Int, Int, Int)] -> String
matrixToString tuples =
  let maxColumn = maxCol tuples
      rows = map (\r -> getRowValues tuples r ++ "\n") [1 .. 9]
  in concat rows


parseLines :: [String] -> Form (Int, Int, Int)
parseLines lines = And (concatMap parseLine (zip [1..] lines))

parseLine :: (Int, String) -> [Clause (Int, Int, Int)]
parseLine (row, line) = concatMap (parseCharacter row) (zip [1..] (filter (/= ' ') line))

parseCharacter :: Int -> (Int, Char) -> [Clause (Int, Int, Int)]
parseCharacter row (col, char)
  | isDigit char = [Or [P (row, col, read [char])]]
  | otherwise    = []


---- testing
-- dpll_sudoku = do{ll <- readLinesToList "./sudoku1.txt"; return $ (dpll (sudoku <&&> (parseLines ll) )) }
-- x = do{ dp <- dpll_sudoku; return $ matrixToString( concat [(findMissingN (findTuples (dp) i j)) | i <- [1..9], j <- [1..9]])}
-- xn = do{ dp <- dpll_sudoku; return $ (findMissingN (findTuples (dp) 1 1))}

--inputLines = [ 
--    "_ _ _ _ _ _ _ _ _" 
--  , "_ _ _ _ _ _ _ _ _"
--  , "_ _ _ _ _ _ _ _ _"
--  , "_ _ _ _ _ _ _ _ _"
--  , "_ _ _ _ _ _ _ _ _"
--  , "_ _ _ _ _ _ _ _ _"
--  , "_ _ _ _ _ _ _ _ _"
--  , "_ _ _ _ _ _ _ _ _"
--  , "_ _ _ _ _ _ _ _ _"
--  ]
