-- 1. allEvens
allEvens :: [Integer] -> [Integer]
allEvens = filter even

allEvens' :: [Integer] -> [Integer]
allEvens' [] = []
allEvens' (l:ls) = if even l then l : allEvens' ls else allEvens' ls

allEvens'' :: [Integer] -> [Integer]
allEvens'' [] = []
allEvens'' (l:ls)
  | even l = l : allEvens'' ls
  | otherwise = allEvens'' ls

-- Other variations start to feel artificial


-- 2. reverse a list
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) [] -- use foldl' to avoid space leak (Data.List)


-- 3. Colour combinations
data Colour = Black | White | Blue | Yellow | Red deriving (Eq, Ord, Show)

-- "values" in haskell are just functions of no parameters
combinations :: [(Colour, Colour)]
combinations = let allColours = [Black, White, Blue, Yellow, Red] in
  [(c,c') | c <- allColours, c' <- allColours, c < c']


-- 4. multiplication tables
multiplicationTable :: [(Integer, Integer, Integer)]
multiplicationTable = [(x,y,x*y) | x <- [1..12], y <- [1..12]]


-- 5. Map colouring
-- Should really create new types for states and colours here, but
-- strings will do

type State = String
type MapColour = String

mapColourings :: [[(State, MapColour)]]
mapColourings = map (zipWith (,) states) $
  [[a,f,g,m,t] | a <- cols, f <- cols, g <- cols, m <- cols, t <- cols,
    m /= t, m /= a, a /= t, a /= m, a /= g, a /= f, g /= f, g /= t]
  where
    cols :: [String]
    cols = ["red", "green", "blue"]

    states :: [State]
    states = ["Alabama", "Florida", "Georgia", "Mississippi", "Tennesse"]

