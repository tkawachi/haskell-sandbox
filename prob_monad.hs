import Data.List (all)
import Data.Ratio

-- リストの各要素が生起確率を持つような型
-- 生起確率は精度落ちしないように Rational を使う。
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

-- Prob を Functor のインスタンスにする。
-- 値に対して f を作用させる。
-- 生起確率はそのままにしておく。
instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs
{-
> fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])
Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}
-}

-- m >>= f は常に join (fmap f m) と等価なので、 join から考える。
-- join という名前は既に使われているので flatten として定義する？
-- 外側の生起確率と内側の生起確率を掛けたものを生起確率とすれば良い。
-- 値はそのまま。
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

instance Monad Prob where
  -- 文脈に入れるときは、必ずそれが起こる (=生起確率 1) で入れる。
  return x = Prob [(x, 1%1)]
  -- m >>= f は join (fmap f m)
  m >>= f = flatten (fmap f m)
  -- パターンマッチに失敗したら
  fail _ = Prob []

{-
モナド第一法則:
return x >>= f と f x が等価。
return x >>= f を展開すると、
Prob [(x,1%1)] >>= f
flatten (fmap f Prob [(x,1%1)])
となる。 f x と等しい気がする。

モナド第二法則:
m >>= return と m が等価。
m >>= return を展開すると、
flatten (fmap return m)

モナド第三法則:
f <=< (g <=< h) と (f <=< g) <=< h が等価。
リストモナドが第三法則を満たし、掛け算が結合法則を満たすので、これも満たされる。

この場合の文脈は「確率が付いている」。
-}

data Coin = Heads | Tails deriving (Show, Eq)

-- 普通のコイン
coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

-- 10回投げると9回裏が出るイカサマコイン
loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

-- イカサマコイン入りの3枚を投げて、全部裏だった確率か否かと確率を返す。
flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (== Tails) [a,b,c])
{-
> flipThree 
Prob {getProb = [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]}

全部裏になるのは 9%40
-}

main = print flipThree
