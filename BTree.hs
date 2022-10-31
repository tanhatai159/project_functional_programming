type IsRoot = Bool

data Tree a = Node {
    listKeys :: [a], 
    listC :: [Tree a], 
    isR :: IsRoot
    } deriving (Show, Eq)

m :: Int
m = 5

maxKeys :: Int
maxKeys = m-1

minKeys :: Int
minKeys = case odd m of
            True -> div m 2
            False -> (div m 2) - 1

insertToList :: Ord a => [a] -> a -> [a]
insertToList [] a = [a]
insertToList (x:xs) a = 
    case compare a x of
        LT -> a:(x:xs)
        _ -> [x] ++ insertToList xs a

-- use for split the list and get middle value
splitMiddle :: [a] -> ([a],Maybe a,[a])
splitMiddle [] = ([],Nothing,[])
splitMiddle (y:ys) = splitMiddle_aux (y:ys) 1 ([],Just y,[])
    where   splitMiddle_aux [] _ _ = ([],Nothing,[])
            splitMiddle_aux (x:xs) n (l,_,r) = case compare n ((div (length (x:xs)) 2) + 1) of
                                                LT -> splitMiddle_aux xs (n+1) (l++[x],Nothing,r)
                                                _ -> (l,Just x,xs)

splitTree :: [Tree a] -> ([Tree a], [Tree a])
splitTree [] = ([],[])
splitTree t = splitTree_aux t 1 ([],[])
    where splitTree_aux (x:xs) n (fstL,sndL) = case compare n ((div (length (x:xs)) 2) + 1) of
                                                LT -> splitTree_aux xs (n+1) (fstL++[x],sndL)
                                                _ -> (fstL++[x],xs) 

-- get tree and index which minimum keys in the tree is less than the input's value 
findOrder :: Ord a => [a] -> [Tree a] -> a -> (Tree a, Int)
findOrder [] _ _ = (Node [] [] True, 1)
findOrder l tl val = findOrder_aux l tl val 1
            -- this 2 case don't care about value of isRoot and use for search at leaf node
    where   findOrder_aux [] [] _ n = (Node [] [] True, n)
            findOrder_aux (a:as) [] input n = case compare input a of
                                                                LT -> ((Node [] [] False), n)
                                                                _ -> findOrder_aux as [] input (n+1)
            -- this 2 case will be used when internal node call
            findOrder_aux [] (b:_) _ n = (b, n)
            findOrder_aux (a:as) (b:bs) input n = case compare input a of
                                                                LT -> (b, n)
                                                                _ -> findOrder_aux as bs input (n+1)

insertInstead :: [a] -> [a] -> Int -> [a]
insertInstead [] t _ = t
insertInstead l t val = insertInstead_aux l t val [] 1
    where insertInstead_aux (x:xs) t n acc order = if n == order
                                                    then acc++t++xs
                                                    else insertInstead_aux xs t n (acc++[x]) (order+1)

getFromList :: (Eq a, Num a) => a -> [b] -> Maybe b
getFromList _ [] = Nothing
getFromList 0 _ = Nothing
getFromList index l = getFromList_aux index l 1
    where   getFromList_aux _ [] _ = Nothing
            getFromList_aux input (x:xs) n = 
                case input == n of
                    True -> Just x
                    False -> getFromList_aux input xs (n+1)

pullFromList :: (Eq b, Num b) => b -> [a] -> (Maybe a, [a])
pullFromList _ [] = (Nothing, [])
pullFromList 0 l = (Nothing, l)
pullFromList index l = pullFromList_aux index l 1 (Nothing, [])
    where   pullFromList_aux _ [] _ acc = acc
            pullFromList_aux input (x:xs) n (val, acc) = case input == n of
                                                        True -> (Just x, acc++xs)
                                                        False -> pullFromList_aux input xs (n+1) (val, acc++[x])

rebalance :: Int -> Tree a -> Tree a -> Either (Tree a) (Tree a)
rebalance order ct t = let listOrder = order-1
                           rightSib = let rightSibM = getFromList (order+1) (listC t)
                                        in case rightSibM of
                                            Nothing -> Node [] [] False
                                            Just x -> x
                           leftSib = let leftSibM = getFromList (order-1) (listC t)
                                        in case leftSibM of
                                            Nothing -> Node [] [] False
                                            Just x -> x
                        in case compare (length (listKeys rightSib)) minKeys of
                            -- can borrow from right sibling
                            GT -> let (rSib_h:rSib_hs) = (listKeys rightSib)
                                      rSib_listC = case (listC rightSib) of
                                                     [] -> []
                                                     _ -> [head (listC rightSib)]
                                      rSib_listCs = case (listC rightSib) of
                                                     [] -> []
                                                     _ -> tail (listC rightSib)
                                    --   (rSib_listC:rSib_listCs) = (listC rightSib)
                                      Just pull_key = getFromList order (listKeys t)
                                      new_listKeys = insertInstead (listKeys t) [rSib_h] order
                                      new_rSib = case rSib_listCs of
                                                    [] -> Node rSib_hs [] (isR rightSib) 
                                                    _ -> Node rSib_hs rSib_listCs (isR rightSib)
                                      new_child_listKeys = (listKeys ct)++[pull_key]
                                      new_child_listC = (listC ct)++rSib_listC
                                      new_child = Node new_child_listKeys new_child_listC (isR ct)
                                      new_listC = insertInstead (listC t) [new_child] order
                                      new_listC2 = insertInstead new_listC [new_rSib] (order+1)
                                    in Right $ Node new_listKeys new_listC2 (isR t)
                            -- can't borrow from right sibling
                            _ -> case compare (length (listKeys leftSib)) minKeys of
                                    -- can borrow from left sibling
                                    GT -> let lSib_t = last (listKeys leftSib)
                                              lSib_ts = init (listKeys leftSib)
                                              lSib_listC = case (listC leftSib) of
                                                            [] -> []
                                                            _ -> [last (listC leftSib)]
                                              lSib_listCs = case (listC leftSib) of
                                                            [] -> []
                                                            _ -> init (listC leftSib)
                                              Just pull_key = getFromList listOrder (listKeys t)
                                              new_listKeys = insertInstead (listKeys t) [lSib_t] listOrder
                                              new_lSib = Node lSib_ts lSib_listCs (isR leftSib)
                                              new_child_listKeys = pull_key:(listKeys ct)
                                              new_child_listC = lSib_listC++(listC ct)
                                              new_child = Node new_child_listKeys new_child_listC (isR ct)
                                              new_listC = insertInstead (listC t) [new_lSib] listOrder
                                              new_listC2 = insertInstead new_listC [new_child] order
                                            in Right $ Node new_listKeys new_listC2 (isR t)
                                    -- can't borrow from right and left sibling so we will merge them
                                    _ -> case listOrder == 0 of
                                            -- merge from left
                                            False -> let (Just new_c, new_listKeys) = pullFromList listOrder (listKeys t)
                                                         new_child_listKeys = (listKeys leftSib)++[new_c]++(listKeys ct)
                                                         new_child_listC = (listC leftSib)++(listC ct)
                                                         new_child = Node new_child_listKeys new_child_listC (isR leftSib)
                                                         new_listC = insertInstead (delFromListIndex order (listC t)) [new_child] listOrder
                                                         new_tree = Node new_listKeys new_listC (isR t)
                                                        in case (isR new_tree) of 
                                                            -- if caller is not root node  
                                                            False -> case compare (length (listKeys new_tree)) minKeys of
                                                                        LT -> Left $ new_tree
                                                                        _ -> Right $ new_tree
                                                            -- if caller is root node
                                                            True -> case (length (listKeys new_tree)) == 0 of
                                                                        False -> Right $ new_tree
                                                                        True -> let new_newTree = head (listC new_tree)
                                                                                 in Right $ Node (listKeys new_newTree) (listC new_newTree) True

                                            -- merge from right
                                            True -> let (Just new_c, new_listKeys) = pullFromList order (listKeys t)
                                                        new_child_listKeys = (listKeys ct)++[new_c]++(listKeys rightSib)
                                                        new_child_listC = (listC ct)++(listC rightSib)
                                                        new_child = Node new_child_listKeys new_child_listC (isR rightSib)
                                                        new_listC = insertInstead (delFromListIndex order (listC t)) [new_child] order
                                                        new_tree = Node new_listKeys new_listC (isR t)
                                                        in case (isR new_tree) of
                                                            -- if caller is not root node  
                                                            False -> case compare (length (listKeys new_tree)) minKeys of
                                                                        LT -> Left $ new_tree
                                                                        _ -> Right $ new_tree
                                                            -- if caller is root node
                                                            True -> case (length (listKeys new_tree)) == 0 of
                                                                        False -> Right $ new_tree
                                                                        True -> let new_newTree = head (listC new_tree)
                                                                                 in Right $ Node (listKeys new_newTree) (listC new_newTree) True

delFromList :: Ord a => a -> [a] -> [a]
delFromList _ [] = []
delFromList val (x:xs)
    | val == x = xs
    | otherwise = let res = delFromList val xs
                    in x:res

getSuccessor :: Ord a => Tree a -> (Maybe a, Either (Tree a) (Tree a))
getSuccessor (Node [] _ isR) = (Nothing, Left $ Node [] [] isR)
getSuccessor (Node allKey l isR) = let (k:listKeys) = allKey
                                       (c:listCs) = l
    in     case l == [] of
            -- if this tree is leaf node
            True -> case compare (length listKeys) minKeys of
                    LT -> (Just k, Left $ Node listKeys [] isR)
                    _ -> (Just k, Right $ Node listKeys [] isR)
            -- if this tree is internal node
            False -> let (Just successor, newTree) = getSuccessor c
                in case newTree of
                        Right t -> let new_child = t:listCs
                                    in (Just successor, Right $ Node allKey new_child isR)
                        Left (Node new_allKey new_childList new_isR) -> let (Node sib_allKeys sib_child sib_isR, _) = findOrder allKey l k
                                                                            (sib_k:sib_listKeys) = sib_allKeys
                                                                            (sib_c:sib_listCs) = sib_child
                                                                        in case compare (length sib_listKeys) minKeys of
                                                                            -- can't brrow from sibling case then we will merge them
                                                                            LT -> let new_firstChild = Node (new_allKey++[k]++sib_allKeys) (new_childList++sib_child) new_isR
                                                                                      new_l = insertInstead listCs [new_firstChild] 1 
                                                                                    in  case compare (length listKeys) minKeys of
                                                                                            LT -> (Just successor, Left $ Node listKeys new_l isR)
                                                                                            _ -> (Just successor, Right $ Node listKeys new_l isR)
                                                                            -- can brrow from siblig case
                                                                            _ -> let new_firstChild = Node (new_allKey++[k]) (new_childList++[sib_c]) new_isR 
                                                                                     new_sib = Node sib_listKeys sib_listCs sib_isR
                                                                                     new_minTree = [new_firstChild, new_sib]
                                                                                     new_l = insertInstead listCs new_minTree 1
                                                                                in (Just successor, Right $ Node (sib_k:listKeys) new_l isR)

getTree :: [Tree a] -> Int -> Tree a
getTree [] _ = Node [] [] False
getTree _ 0 = Node [] [] False
getTree lT n = case compare n m of
                GT -> Node [] [] False
                _ -> getTree_aux lT n 1
    where getTree_aux (x:xs) input order = case input == order of
                                            True -> x
                                            False -> getTree_aux xs input order

delFromListIndex :: (Eq a, Num a) => a -> [b] -> [b]
delFromListIndex _ [] = []
delFromListIndex index l = delFromListIndex_aux index l 1
    where delFromListIndex_aux input (x:xs) n = 
            case input == n of
                True -> xs
                False -> let res = delFromListIndex_aux input xs (n+1)
                            in (x:res)

search :: Ord a => Tree a -> a -> Bool
search (Node [] _ _) _ = False
search (Node t c _) val = 
    case elem val t of
        True -> True
        False -> let (childT, _) = findOrder t c val
                    in search childT val


insert :: Ord a => a -> Tree a -> Either (Tree a) (Tree a)
insert val (Node [] _ _) = Right $ Node [val] [] True
insert val t@(Node listKeys listC isR) = 
    case search t val of
        True -> Left t
        False -> case listC of
                    -- if tree is leaf node
                    [] -> let new_listKeys = insertToList listKeys val
                            in case compare (length new_listKeys) (maxKeys) of
                                GT -> let (leftC,Just mid,rightC) = splitMiddle new_listKeys
                                    in case isR of
                                        True -> Right $ Node [mid] [Node leftC [] False, Node rightC [] False] isR
                                        False -> Left $ Node [mid] [Node leftC [] False, Node rightC [] False] isR
                                _ -> Right $ Node new_listKeys listC isR
                    -- if tree is internal node
                    _ -> let (childT, order) = findOrder listKeys listC val
                        in case insert val childT of
                            Right t -> Right $ Node listKeys (insertInstead listC [t] order) isR
                            Left (Node (mid:_) listC_child isR_child) -> let new_listKeys = insertToList listKeys mid
                                                                             new_listC = insertInstead listC listC_child order
                                                                        in case compare (length new_listKeys) maxKeys of
                                                                            GT -> let (new_left, Just new_mid, new_right) = splitMiddle new_listKeys
                                                                                      (childOfLeft, childOfRight) = splitTree new_listC
                                                                                    in case isR of
                                                                                        True -> Right $ Node [new_mid] [Node new_left childOfLeft False, Node new_right childOfRight False] isR
                                                                                        False -> Left $ Node [new_mid] [Node new_left childOfLeft False, Node new_right childOfRight False] isR
                                                                            _ -> Right $ Node new_listKeys new_listC isR

delete :: Ord a => a -> Tree a -> Either (Tree a) (Tree a)
delete _ (Node [] _ _) = Left $ Node [] [] True
delete val t = 
    case search t val of
        -- if the tree don't have value that we want to delete
        False -> Left $ t
        -- if the tree have value that we want to delete
        True -> case elem val (listKeys t) of
            -- if this tree has want value
            True -> case (listC t) of
                -- if this tree is leaf node
                [] -> let new_listKeys = delFromList val (listKeys t)
                        in case compare (length new_listKeys) minKeys of
                            LT -> case (isR t) of
                                    True -> Right $ Node new_listKeys [] (isR t)
                                    False -> Left $ Node new_listKeys [] (isR t)
                            _ -> Right $ Node new_listKeys [] (isR t)
                -- if this tree is internal node
                _ -> let (successorT, order) = findOrder (listKeys t) (listC t) val
                         listOrder = order-1
                         (Just successor, succT) = getSuccessor successorT 
                        --  new_listKeys = insertInstead (listKeys t) [rSib_h] listOrder 
                     in case succT of
                            -- tree from successor is already balance
                            Right newT -> let new_child = insertInstead (listC t) [newT] order
                                              new_listKeys = insertInstead (listKeys t) [successor] listOrder
                                            in Right $ Node new_listKeys new_child (isR t)
                            -- tree from successor is not balance 
                            Left newT -> let new_listKeys = insertInstead (listKeys t) [successor] listOrder
                                             new_tree = Node new_listKeys (listC t) (isR t)
                                            in rebalance order newT new_tree 
            -- if this tree doesn't has want value
            False -> let (tree, order) = findOrder (listKeys t) (listC t) val
                         del_treeM = delete val tree
                    in case del_treeM of
                        Right del_tree -> let new_listC = insertInstead (listC t) [del_tree] order
                                            in Right $ Node (listKeys t) new_listC (isR t)
                        Left del_tree -> rebalance order del_tree t

-- data Tree a = Empty | Node [a] (Tree a) (Tree a) (Tree a) IsRoot deriving (Show, Eq)

-- mydiv :: Integral a => a -> a -> a
-- mydiv a b = case odd a of
--              True -> (div a b) + 1
--              False -> div a b

-- maxChild :: Int
-- maxChild = m

-- minChild :: Int
-- minChild = mydiv m 2

ls1 = [1,2,3,4,5]
ls2 = [1,2,3]

sT = Node [] [] True
t4 = Right (Node [1,2,3,4] [] True)
t5 = Right (Node [3] [Node [1,2] [] False,Node [4,5] [] False] True)
t7 = Right (Node [7] [Node [1,2] [] False,Node [10,20,30,40] [] False] True)
t8 = Right (Node [3,6] [Node [1,2] [] False,Node [4,5] [] False,Node [7,8] [] False] True)
t10 = Right (Node [3,6] [Node [1,2] [] False,Node [4,5] [] False,Node [7,8,9,10] [] False] True)
t11 = Right (Node [3,6,9] [Node [1,2] [] False,Node [4,5] [] False,Node [7,8] [] False,Node [10,11] [] False] True)
t16 = Right (Node [3,6,9,12] [Node [1,2] [] False,Node [4,5] [] False,Node [7,8] [] False,Node [10,11] [] False,Node [13,14,15,16] [] False] True)
t17 = Right (Node [9] [Node [3,6] [Node [1,2] [] False,Node [4,5] [] False,Node [7,8] [] False] False,Node [12,15] [Node [10,11] [] False,Node [13,14] [] False,Node [16,17] [] False] False] True)

demo1 = Node [100] [Node [30,60] [Node [15,18] [] False,Node [31,33,47] [] False,Node [62,73,84] [] False] False,Node [200,300] [Node [101,120,130] [] False,Node [202,210,250] [] False,Node [304,350] [] False] False] True
demo2 = Node {listKeys = [100], listC = [Node {listKeys = [30,60], listC = [Node {listKeys = [15,18], listC = [], isR = False},
                                                                            Node {listKeys = [31,33,47], listC = [], isR = False},
                                                                            Node {listKeys = [62,73,84], listC = [], isR = False}], isR = False},
                                        Node {listKeys = [200,300], listC = [Node {listKeys = [101,120,130], listC = [], isR = False},
                                                                            Node {listKeys = [202,210,250], listC = [], isR = False},
                                                                            Node {listKeys = [304,350], listC = [], isR = False}], isR = False}], isR = True}

testTree = (Node [7] [Node [1,3] [] False,Node [10,20,30,40] [] False] True)

demo3 = Node {listKeys = [10], listC = [Node {listKeys = [3,7], listC = [], isR = False},
                                        Node {listKeys = [20,30,40], listC = [], isR = False},
                                        Node {listKeys = [10,20,30,40], listC = [], isR = False}], isR = True}