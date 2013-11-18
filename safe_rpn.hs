-- 逆ポーランド記法の電卓 (誤った入力に対しては Nothing を返す)
import Data.List
import Control.Monad

-- reads は読み取りに成功した時、[(読み取り結果, 消費しきれなかった文字列)] を返す。
-- 読み込み失敗時は空リストを返す。
-- reads :: Read a => String -> [(a, String)]
--
-- > reads "123" :: [(Double, String)]
-- [(123.0,"")]
-- > reads "123abc" :: [(Double, String)]
-- [(123.0,"abc")]
-- > reads "abc" :: [(Double, String)]
-- []
--
-- 失敗した場合を空リストで表しているので、代わりに Maybe を使って
-- Read a => String -> Maybe (a, String) の型のほうが自然な気がする。

-- 文字列を全部読み取れたら Just, それ以外の場合は Nothing を返す。
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

solveRPN :: String -> Maybe Double
solveRPN st = do
  -- Maybe [Double] を bind するときに pattern match している。
  -- Nothing のときや2以上の要素があるときは result が Nothing になる。
  [result] <- foldM foldingFunction [] (words st)
  return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return $ (y * x):ys
foldingFunction (x:y:ys) "+" = return $ (y + x):ys
foldingFunction (x:y:ys) "-" = return $ (y - x):ys
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)
-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- Functor でいうところの fmap

{-
main = do
  line <- getLine
  putStrLn $ show $ solveRPN line
  main
-}
