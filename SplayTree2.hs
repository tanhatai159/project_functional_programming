data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)
data Pattern = L | R deriving (Show)

splay :: Ord a => Tree a -> a -> [Pattern] -> (Tree a, Either [Pattern] [Pattern])
splay Empty _ _ = (Empty, Right [])
splay (Node l x r) input accPattern 
    | input == x = ((Node l x r), Left accPattern)
    | input > x = let ((Node rl rx rr), pattern) = splay r input (R:accPattern)
                    in case pattern of
                        Left p -> case compare (length p) 1 of
                                    GT -> ((Node l x r), Right p)
                                    -- this case can be only 'EQ' but to prevent yellow line I have to use underscore
                                    _ -> let t1 = (Node l x rl)
                                            in ((Node t1 rx rr), Right [])
                        Right p -> let (a:as) = p
                                       (b:leftPatt) = as
                                       currentPatt = b:[a]
                                    in case currentPatt of
                                        [R,R] -> let (Node cr_rl cr_rx cr_rr) = rr
                                                     t1 = (Node l x rl)
                                                     t2 = (Node t1 rx cr_rl)
                                                in ((Node t2 cr_rx cr_rr), Left leftPatt)
                                    -- this case can be only [R,L] but to prevent yellow line I have to use underscore
                                        _ -> let (Node cl_rl cl_rx cl_rr) = rl
                                                 t1 = (Node l x cl_rl)
                                                 t2 = (Node cl_rr rx rr)
                                             in ((Node t1 cl_rx t2), Left leftPatt)
    | otherwise = let ((Node ll lx lr), pattern) = splay l input (L:accPattern)
                    in case pattern of
                        Left p -> case compare (length p) 1 of
                                    GT -> ((Node l x r), Right p)
                                    -- this case can be only 'EQ' but to prevent yellow line I have to use underscore
                                    _ -> let t1 = (Node lr x r)
                                            in ((Node ll lx t1), Right [])
                        Right p -> let (a:as) = p
                                       (b:leftPatt) = as
                                       currentPatt = b:[a]
                                    in case currentPatt of
                                        [L,L] -> let (Node cl_ll cl_lx cl_lr) = ll
                                                     t1 = (Node lr x r)
                                                     t2 = (Node cl_lr lx t1)
                                                in ((Node cl_ll cl_lx t2), Left leftPatt)
                                    -- this case can be only [L,R] but to prevent yellow line I have to use underscore
                                        _ -> let (Node cr_ll cr_lx cr_lr) = lr
                                                 t1 = (Node ll lx cr_ll)
                                                 t2 = (Node cr_lr x r)
                                                  in ((Node t1 cr_lx t2), Left leftPatt)

--use to check if specific value is in the tree.
check :: Ord a => Tree a -> a -> Bool
check Empty _ = False
check (Node l x r) input
    | input == x = True
    | input > x = check r input
    | otherwise = check l input

findMax :: Ord a => Tree a -> Maybe a
findMax Empty = Nothing
findMax (Node _ x r)
    | r == Empty = Just x
    | otherwise = findMax r

findMin :: Ord a => Tree a -> Maybe a
findMin Empty = Nothing
findMin (Node l x _)
    | l == Empty = Just x
    | otherwise = findMin l

search :: Ord a => Tree a -> a -> Either (Tree a) (Tree a)
search Empty _ = Left Empty
search t input = 
    case check t input of
        True -> let (newTree, _) = splay t input []
                in Right newTree
        False -> let lastLeaf = searchLast t input
                     (newTree, _) = splay t lastLeaf []
                 in Left newTree
    where searchLast (Node l x r) val = case compare val x of
                                            GT -> case r == Empty of
                                                    True -> x
                                                    False -> searchLast r val
                                            _ -> case l == Empty of
                                                    True -> x
                                                    False -> searchLast l val

insert :: Ord a => Tree a -> a -> Tree a
insert t input =
    case check t input of
        True -> let (reTree, _) = splay t input []
                in reTree
        False -> let inTree = insert_aux t input
                     (reTree, _) = splay inTree input []
                 in reTree 
    where insert_aux Empty n = Node Empty n Empty
          insert_aux (Node l x r) n
            | input > x = Node l x (insert_aux r n)
            | otherwise = Node (insert_aux l input) x r

delete :: (Ord a) => Tree a -> a -> Tree a
delete Empty _ = Empty
delete t input = 
    case search t input of
        Left newTree -> newTree
        Right (Node l _ r) -> case l of
                                Empty -> r
                                _ -> let Just maxVal = findMax l
                                         (Node nl nx _, _) = splay l maxVal []
                                        in Node nl nx r
    -- case check t input of
    --     False -> let Left newTree = search t input
    --              in newTree
    --     True -> let Right (Node l _ r) = search t input
    --             in case l of
    --                 Empty -> r
    --                 _ -> let Just maxVal = findMax l
    --                          (Node nl nx _, _) = splay l maxVal []
    --                      in Node nl nx r   

-- treeFold :: (b -> a -> b) -> b -> Tree a -> b
-- treeFold _ acc Empty = acc
-- treeFold f acc (Node l x r) = 
--     let acc2 = treeFold f acc l
--         acc3 = f acc2 x
--     in treeFold f acc3 r 

-- findMin :: Ord a => Tree a -> Maybe a
-- findMin Empty = Nothing
-- findMin t = treeFold cp Nothing t
--     where cp acc x = 
--             case acc of
--                 Nothing -> Just x
--                 Just a -> if x < a then Just x else Just a


-- findMax :: Ord a => Tree a -> Maybe a
-- findMax Empty = Nothing
-- findMax t = treeFold cp Nothing t
--     where cp acc x = 
--             case acc of
--                 Nothing -> Just x
--                 Just a -> if x > a then Just x else Just a

-- check2 :: Ord a => Tree a -> a -> Bool
-- check2 Empty _ = False
-- check2 t input = treeFold isEqual False t
--     where isEqual acc a
--             | input == a = True
--             | otherwise = acc


testTree1 = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)
-- testTree2 = Node (Node (Node Empty 1 Empty) 3 (Node Empty 2 Empty)) 5 (Node Empty 4 Empty)
testTree3 = Node (Node 
                    (Node 
                        (Node 
                            (Node Empty 1 Empty) 
                            2 
                            Empty) 
                        3 
                        Empty) 
                    4 
                    Empty) 
                50 
                Empty 

demo1 = Node (Node 
                (Node Empty 7 Empty) 
                10 
                (Node Empty 11 Empty) ) 
            20 
            (Node 
                (Node Empty 25 Empty) 
                30 
                (Node Empty 36 Empty) )

demo2 = Node (Node 
                (Node Empty 7 Empty) 
                10 
                (Node Empty 11 Empty)) 
            20 
            (Node 
                Empty 
                30 
                (Node Empty 36 Empty))
