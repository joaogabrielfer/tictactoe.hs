import Control.Monad (foldM)
import Data.Either (isLeft, lefts)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)

data Cell = X | O | Nil deriving (Eq)

instance Show Cell where
  show X = "X"
  show O = "O"
  show Nil = " "

newtype Board = Board [[Cell]]

instance Show Board where
  show (Board board) = intercalate "\n" (map (intercalate " | " . map show) board)

initialBoard = Board [[Nil | _ <- [0 .. 2]] | _ <- [0 .. 2]]

updateRow :: [Cell] -> Int -> Cell -> [Cell]
updateRow row x cell = case splitAt x row of
  (before, _ : after) -> before ++ (cell : after)
  _ -> row

setCell :: (Int, Int) -> Cell -> Board -> Board
setCell (x, y) cell (Board board) = case splitAt y board of
  (before, curr : after) -> Board $ before ++ (updateRow curr x cell : after)
  _ -> Board board

checkRow :: Board -> Int -> Either Cell Board
checkRow (Board board) y = case splitAt y board of
  (_, [a, b, c] : _)
    | a == b && b == c && a /= Nil -> Left a
  _ -> Right (Board board)

checkCol :: Board -> Int -> Either Cell Board
checkCol (Board board) x = case map (splitAt x) board of
  [(_, a : _), (_, b : _), (_, c : _)]
    | a == b && b == c && a /= Nil -> Left a
  _ -> Right (Board board)

checkDiag :: Board -> Int -> Either Cell Board
checkDiag (Board board) dir =
  case foldl
    (\acc r -> acc >>= \l -> if l == r then Just r else Nothing)
    (Just (board !! (if dir > 0 then 2 else 0) !! 0))
    [board !! (if dir > 0 then (2 - i) else i) !! i | i <- [0 .. length board - 1]] of
    Just c -> Left c
    Nothing -> Right (Board board)

checkAll :: Board -> (Board -> Int -> Either Cell Board) -> Either Cell Board
checkAll board check = foldr step (Right board) $ map (check board) [0 .. 2]
  where
    step (Left x) _ = Left x
    step (Right _) acc = acc

checkWin :: Board -> Either Cell Board
checkWin board = foldM checkAll board [checkCol, checkRow, checkDiag]
