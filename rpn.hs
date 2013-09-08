-- 逆ポーランド記法の電卓 (正しい入力である限り動く)
import Data.List

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
           where foldingFunction (x:y:ys) "*" = (x * y):ys
                 foldingFunction (x:y:ys) "+" = (x + y):ys
                 foldingFunction (x:y:ys) "-" = (x - y):ys
                 foldingFunction xs numberString = read numberString:xs

main = do
  line <- getLine
  putStrLn $ show $ solveRPN line
  main
