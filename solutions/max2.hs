{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage [] = Unknown ""
parseMessage (x:xs)
    | elem x "IW" = LogMessage
                        Info
                        ((read . head . words $ xs ) :: Int)
                        (unwords . drop 1 . words $ xs)
    | x == 'E' = LogMessage
                        (Error (read (head . words $ xs) :: Int))
                        (read (head . drop 1 . words $ xs) :: Int)
                        (unwords . drop 2 . words $ xs)
    | otherwise = Unknown (x:xs)

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert (LogMessage lmT lmTi lmSt) (Node smaller (LogMessage a nTi x) bigger)
    | lmTi < nTi = Node
                    (insert (LogMessage lmT lmTi lmSt) smaller)
                    (LogMessage a nTi x)
                    bigger
    | otherwise = Node
                    smaller
                    (LogMessage a nTi x)
                    (insert (LogMessage lmT lmTi lmSt) bigger)
insert lm Leaf = Node Leaf lm Leaf
insert (LogMessage _ _ _) (Node _ (Unknown _) _) = Leaf

build :: [LogMessage] -> MessageTree
build messages = foldr insert Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node small lm big) = inOrder small ++ lm : inOrder big

strip :: LogMessage -> [String]
strip (LogMessage (Error sev) _ str)
    | sev > 50 = [str]
    | otherwise = []
strip _ = []

fluffer :: [LogMessage] -> [[String]]
fluffer list = map strip list

stripper :: [[String]] -> [String]
stripper list = foldl (++) [] list

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = stripper $ fluffer $ inOrder (build messages)