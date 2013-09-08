-- 逆ポーランド記法の電卓 (誤った入力に対しては Nothing を返す)
import Data.List
import Control.Monad

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return $ (y * x):ys
foldingFunction (x:y:ys) "+" = return $ (y + x):ys
foldingFunction (x:y:ys) "-" = return $ (y - x):ys
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

main = do
  line <- getLine
  putStrLn $ show $ solveRPN line
  main
