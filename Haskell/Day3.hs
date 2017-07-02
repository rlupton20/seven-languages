-- Hash tables

-- Our hash will be modelled by a tree (not balanced, but
-- that isn't crucial for the exercise. Left branches are
-- smaller hashes, right branches larger
data Hash k a = End | Node (Hash k a) (k,a) (Hash k a) deriving (Eq, Show)

insert :: (Ord k) => (k,a) -> Hash k a -> Hash k a
insert (k,v) End = Node End (k,v) End
insert (k,v) (Node l (k',v') r)
  | k < k' = Node (insert (k,v) l) (k',v') r
  | k > k' = Node l (k',v') (insert (k,v) r)
  | otherwise = Node l (k,v) r

exampleHash :: Hash Int String
exampleHash = insert (3, "string") . insert (2, "hash") . insert (5, "map") $ End

hashLookup :: (Ord k, Eq a) => k -> Hash k a -> Maybe a
hashLookup _ End = Nothing
hashLookup k (Node l (k',v) r)
  | k < k' = hashLookup k l
  | k > k' = hashLookup k r
  | otherwise = Just v


hyperHash :: Hash Int (Hash Int (Hash Int String))
hyperHash = embed . embed $ exampleHash
  where
    embed :: Hash Int a -> Hash Int (Hash Int a)
    embed h = insert (3, h) . insert (2, h) . insert (5,h) $ End

exampleMonadicLookup :: Maybe String
exampleMonadicLookup = hashLookup 3 hyperHash >>= hashLookup 2 >>= hashLookup 5


-- Mazes
-- Given the list monad solving problem, it would be much
-- easier to represent a maze as an augmented tree. Let's start
-- with the suggested interface. A maze will be a hashmap of nodes

data MazeNode = Exit | Connect [(Int,Int)] deriving (Eq, Show)
type Maze = Hash (Int,Int) MazeNode

dumbMaze :: Maze
dumbMaze = insert ((0,0),Connect [(1,0)]) .
  insert ((1,0), Connect [(1,1), (1,2)]) .
  insert ((1,1), Connect []) .
  insert ((1,2), Exit) $ End

getNode :: (Int,Int) -> Maze -> Maybe MazeNode
getNode = hashLookup

solveMaze :: (Int,Int) -> Maze -> [[(Int,Int)]]
solveMaze from maze = fmap reverse $ trackRoute [] from
  where
    trackRoute :: [(Int,Int)] -> (Int,Int) -> [[(Int,Int)]]
    trackRoute route from = let node = getNode from maze in
      if node == Just Exit then return route else
        do
          adjacent <- case node of
            Just (Connect l) -> l
            _ -> []
          trackRoute (adjacent : route) adjacent
