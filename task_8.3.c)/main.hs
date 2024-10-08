z :: [Int] -> Int
z x = 0

n :: [Int] -> Int
n x = (head x) + 1

at :: [Int] -> Int -> [Int] -> Int
at count k x
  | head count == k = head x
  | otherwise = at([n(count)])(k)(tail x)

u :: Int -> Int -> [Int] -> Int
u n k x = at([1])(k)(x)

mapFuns :: [[Int] -> Int] -> [Int] -> [Int]
mapFuns fs l = ($ l) <$> fs

s :: ([Int] -> Int) -> [[Int] -> Int] -> [Int] -> Int
s g f x = g(mapFuns(f)(x))

r :: ([Int] -> Int) -> ([Int] -> Int) -> [Int] -> Int -> Int
r f g x y
  | y == 0 = f(x)
  | otherwise = g(x++[y - 1, r(f)(g)(x)(y - 1)])


plus :: [Int] -> Int
plus x = r(u(1)(1))(s(n)[(u(3)(3))])([head x])(head(tail x))

multiply :: [Int] -> Int
multiply x = r(z)(s(plus)[(u(3)(1)),(u(3)(3))])([head x])(head(tail x))

factorial :: [Int] -> Int
factorial a = r(s(n)[z])(s(multiply)[u(3)(3), s(n)[u(3)(2)]])(a)(head a)

a :: Int
a = factorial[9]

main :: IO ()
main = do
  print a
