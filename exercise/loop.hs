module Exercise where

-- http://vipprog.net/wiki/exercise.html#eb2c4338
-- Hello World![改行]を5回表示させてください。
-- print(或いはprintf,cout等)を5回コピーすれば当然可能ですが、
-- ループ構文(for,while等)を利用して、print等は1回の使用にとどめてみてください。

times :: Monad m => Int -> m() -> m()
times 0 action = return ()
times n action = action >> times (n - 1) action

hello5 :: IO()
hello5 = 5 `times` putStrLn "Hello World!"
