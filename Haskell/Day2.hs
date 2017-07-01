import Data.Char (ord)


-- 1. Everyone probably quicksorts, so lets merge sort instead
mergeSort :: (Ord a) => [a] -> [a]
mergeSort = head . until hasLengthOne mergePairs . unitize
  where
    mergeTwo :: (Ord a) => [a] -> [a] -> [a]
    mergeTwo [] ls = ls
    mergeTwo ls [] = ls
    mergeTwo x'@(x:xs) y'@(y:ys)
      | x < y = x : mergeTwo xs y'
      | otherwise = y : mergeTwo x' ys

    mergePairs :: (Ord a) => [[a]] -> [[a]]
    mergePairs [] = []
    mergePairs [x] = [x]
    mergePairs (x:xs:xss) = mergeTwo x xs : mergePairs xss

    hasLengthOne :: [a] -> Bool
    hasLengthOne = (==1) . length

    until :: (a -> Bool) -> (a -> a) -> a -> a
    until test f x = case test x of
      True -> x
      False -> until test f $ f x

    unitize :: [a] -> [[a]]
    unitize = map (\x -> [x])


-- 2. The Ord type class is really an ordering function parameter that the
-- compiler resolves, but lets make the couple of character modification
-- needed to "generalize" the above mergeSort. ScopedTypeVariables would
-- make this even easier
mergeSortWith :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
mergeSortWith cmp = head . until hasLengthOne (mergePairs cmp) . unitize
  where
    mergeTwo :: (Ord a) => (a -> a -> Bool) -> [a] -> [a] -> [a]
    mergeTwo _ [] ls = ls
    mergeTwo _ ls [] = ls
    mergeTwo cmp x'@(x:xs) y'@(y:ys)
      | cmp x y = x : mergeTwo cmp xs y'
      | otherwise = y : mergeTwo cmp x' ys

    mergePairs :: (Ord a) => (a -> a -> Bool) -> [[a]] -> [[a]]
    mergePairs _ [] = []
    mergePairs _ [x] = [x]
    mergePairs cmp (x:xs:xss) = mergeTwo cmp x xs : mergePairs cmp xss

    hasLengthOne :: [a] -> Bool
    hasLengthOne = (==1) . length

    until :: (a -> Bool) -> (a -> a) -> a -> a
    until test f x = case test x of
      True -> x
      False -> let x' = f x in x' `seq` (until test f $ x')

    unitize :: [a] -> [[a]]
    unitize = map (\x -> [x])


-- 3. Rubbish price parser
-- (rubbish because we don't really deal with error cases)

-- By recursion
parsePrice :: String -> Double
parsePrice = readNumber . drop 1
  where
    readNumber :: String -> Double
    readNumber num = let (d,e) = toDoubleParts (0,0) id num in
      (fromIntegral d) / (10.0 ** (fromIntegral $ -e))

    toDoubleParts :: (Integer, Integer) ->
                     (Integer -> Integer) ->
                     String ->
                     (Integer, Integer)
    toDoubleParts t _ "" = t
    toDoubleParts (d,e) expmod (n:ns)
        | n == '.' = toDoubleParts (d,0) (\x -> x-1) ns
        | otherwise = toDoubleParts (recordDigit d n, expmod e) expmod ns

    recordDigit :: Integer -> Char -> Integer
    recordDigit d c = d * 10 + (toInteger $ charToInt c)

    charToInt :: Char -> Int
    charToInt c = ord c - ord '0'


parsePrice' :: String -> Double
parsePrice' price = let (b,s) = getDigits (drop 1 price) in
  writeNumber b + (writeNumber s / (10 ** ( fromIntegral $ length s )))
  where
    writeNumber :: [Int] -> Double
    writeNumber = foldl (\a v -> a*10 + fromIntegral v) 0

    getDigits :: String -> ([Int], [Int])
    getDigits = foldr f ([],[])
      where
        f = (\c (l,r) -> if c == '.' then (r,l) else (charToInt c : l, r))

    charToInt :: Char -> Int
    charToInt c = ord c - ord '0'

-- (could also just use read)
parsePrice'' :: String -> Double
parsePrice'' = read . drop 1


-- 4. Lazy sequences
-- Either this is badly phrased or just not possible. Taking x,y=0, we would
-- have functions generating multiples of 3 and 5. The function types don't
-- even make sense for what is needed

-- 5. Partial functions
half :: Double -> Double
half = (/2)

addNewline :: String -> String
addNewline = (++"\n")


-- 6. GCD
gcd' :: Integer -> Integer -> Integer
gcd' x y
  | y < x = gcd y x
  | x `mod` y == 0 = y
  | otherwise = gcd (x `mod` y) y

-- 7. Lazy primes
primes = filterPrimes [2..]
  where
    filterPrimes (p:ps) = p : filterPrimes [q | q <- ps, q `mod` p /= 0]

-- 8. Line splitter
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\t' = True
isSpace _ = False

untilWord :: String -> Int
untilWord ss = go ss 0
  where
    go "" n = n
    go (s:ss) n
      | isSpace s = let m = n + 1 in m `seq` go ss m
      | otherwise = n

untilSpace :: String -> Int
untilSpace ss = go ss 0
  where
    go "" n = n
    go (s:ss) n
      | isSpace s = n
      | otherwise = let m = n + 1 in m `seq` go ss m

untilNextSpace :: String -> Int
untilNextSpace s = let spaces = untilWord s in
  spaces + untilSpace (drop spaces s)

splitLine :: Int -> String -> [String]
splitLine limit ss = go ss [] "" 0
  where
    go "" ts "" _ = reverse ts
    go "" ts l _ = reverse $ l : ts
    go s ts l c
      | c == 0 && untilSpace s > limit =
          let notSpace = not . isSpace
              dropUntilNextWord = dropWhile isSpace . dropWhile notSpace
          in
            go (dropUntilNextWord s) (takeWhile notSpace s : ts) "" 0
      | c + untilNextSpace s > limit = go (dropWhile isSpace s) (l:ts) "" 0
      | otherwise = let jump = untilNextSpace s in
          go (drop jump s) ts (l ++ (take jump s)) (c + jump)

splitText :: Int -> String -> [String]
splitText limit = concatMap (splitLine limit) . lines

-- Split text with line numbers
numberedSplitText :: Int -> String -> [(Integer, String)]
numberedSplitText limit = zip [0..] . splitText limit
