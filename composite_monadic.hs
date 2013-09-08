import Data.List
import Control.Monad

-- ただの関数合成 .
f = (+1) . (*100)
{-
> f 4
401
-}


-- モナディック関数の合成 <=<
-- テキストでは型宣言が無いが、型宣言が無いとコンパイルエラーになる。
g :: (Monad m, Num a) => a -> m a
g = (\x -> return (x + 1)) <=< (\x -> return (x * 100))
{-
> g 4 :: Maybe Int
Just 401
> g 4 :: [Int]
[401]
> g 4
401
-}

-- 関数リストを全部合成
-- 初期値に id (渡されたものをそのまま返す関数)、foldr で畳み込み。
-- (((引き数 + 1) * 100) + 8) を作る。
f' = foldr (.) id [(+8),(*100),(+1)]
{-
> f' 1
208
-}

-- モナディック関数リストを全部合成
-- . を >=> に、 id を return にすれば OK。

-- Knight の場所
type KnightPos = (Int, Int)
-- Knight が動ける場所
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
              (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')
-- Knightが3手で動ける場所
in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
-- 3手でいけるか？
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
{-
> canReachIn3 (1, 3) (1, 6)
True
> canReachIn3 (1, 3) (1, 5)
False
-}

-- 上記 in3, canReachIn3 を「n手で動ける場所」「n手でいけるか？」に一般化したい。
inMany :: Int -> KnightPos -> [KnightPos]
-- moveKnight が x 個入ったリストを作る (replicate x moveKnight)
-- . の代わりに <=<、 id の代わりに return を使って foldr で関数合成。
-- できた関数は moveKnight と同じ型 (KnightPos -> [KnightPos])
-- 初期位置が入った一要素リストを return で作って bind。
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start
{-
> canReachIn 3 (1, 3) (1, 6)
True
> canReachIn 3 (1, 3) (1, 5)
False
-}

main = do
  putStrLn $ show $ canReachIn 3 (1, 3) (1, 6)
