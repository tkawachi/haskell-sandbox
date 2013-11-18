-- 逆ポーランド記法の電卓 (正しい入力である限り動く)
import Data.List

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words

foldingFunction :: [Double] -> String -> [Double]
foldingFunction (x:y:ys) "*" = (y * x):ys
foldingFunction (x:y:ys) "+" = (y + x):ys
foldingFunction (x:y:ys) "-" = (y - x):ys
foldingFunction xs numberString = read numberString:xs

main = do
  line <- getLine
  print $ solveRPN line
  main
