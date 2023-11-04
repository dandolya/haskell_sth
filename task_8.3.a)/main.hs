z :: [Int] -> Int
z x = 0

n :: [Int] -> Int
n x = 1 + head x

at :: [Int] -> Int -> [Int] -> Int
at count k x
  | head count == k = head x
  | otherwise = at [n count] k $ tail x

u :: Int -> Int -> [Int] -> Int
u n k x = at [1] k x

mapFuns :: [[Int] -> Int] -> [Int] -> [Int]
mapFuns fs l = ($ l) <$> fs

s :: ([Int] -> Int) -> [[Int] -> Int] -> [Int] -> Int
s g f x = g $ mapFuns f x

r :: ([Int] -> Int) -> ([Int] -> Int) -> [Int] -> Int -> Int
r f g x y
  | y == 0 = f x
  | otherwise = g $ x ++ [y - 1, r f g x $ y - 1]

plus :: [Int] -> Int
plus x = r (u 1 1) (s n [u 3 3]) [head x] $ head $ tail x

multiply :: [Int] -> Int
multiply x = r z (s plus [u 3 1, u 3 3]) [head x] $ head $ tail x

dec :: [Int] -> Int
dec a = r (u 1 1)(u 3 2) a $ head a

limited_minus :: [Int] -> Int
limited_minus a = r (u 1 1) (s dec [u 3 3]) [head a] $ head $ tail a

a :: Int
a = limited_minus[30, 25]

main :: IO ()
main = do
  print a
