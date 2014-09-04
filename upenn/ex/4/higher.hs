fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer

fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer

fun2' = sum . filter even . takeWhile (>1) . iterate hailstorm

hailstorm n | even n = n `div` 2
            | otherwise = 3 * n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

foldTree :: (Ord a) => [a] -> Tree a

foldTree = foldr addValueToTree Leaf

addValueToTree value Leaf = makeNode value Leaf Leaf

addValueToTree value (Node h l v r)
    | value >= v = makeNode v l (addValueToTree value r)
    | otherwise  = makeNode v (addValueToTree value l) r

height Leaf = 0

height (Node h _ _ _) = h

makeNode value left right = Node (1 + max (height left) (height right)) left value right

-- incoming node is the root node, pivot node is its
-- right subtree (because this is a left rotation)
rotateLeft (Node _ rL rV (Node _ pL pV pR)) =
            let newRootLeft = makeNode rV rL pL
                newRootRight = pR
            in  makeNode pV newRootLeft newRootRight

-- incoming node is the root node, pivot node is its
-- left subtree (because this is a right rotation)
rotateRight (Node _ (Node _ pL pV pR) rV rR) =
            let newRootLeft = pL
                newRootRight = makeNode rV pR rR
            in  makeNode pV newRootLeft newRootRight
