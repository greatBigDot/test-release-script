module Data.ContextFree
  ( Symbol(Terminal, NonTerminal)
  , Phrase(Phrase)
  , Rule
  , mkRule
  , isTerminal
  , apply, derive
  , reduce, limitedReduce
  , start
  ) where

data Symbol = Terminal String | NonTerminal String deriving Eq
newtype Phrase = Phrase { symbols :: [Symbol] } deriving Eq
newtype Rule  = Rule (Symbol,Phrase) deriving Eq

instance Show Symbol where
  show (Terminal s) = s
  show (NonTerminal s) = s

instance Show Phrase where
  show = concatMap show . symbols

instance Show Rule where
  show (Rule (hd,body)) = "(" ++ show hd ++ ") --> (" ++ show body ++ ")"

-- rHead :: Rule -> Symbol

mkRule :: (Symbol,Phrase) -> Rule
mkRule specs
  | isValidRule specs = Rule specs
  | otherwise         = error "Invalid rule specification."

isValidRule :: (Symbol,Phrase) -> Bool
isValidRule (hd,_) = not . isTerminal $ hd

isTerminal :: Symbol -> Bool
isTerminal (Terminal _) = True
isTerminal _            = False

isDone :: Phrase -> Bool
isDone = all isTerminal . symbols


--replaces the nth element of xs with ys; expand [30,20,10] [0..10] 7 ==> [0,1,2,3,4,5,30,20,10,7,8,9,10]
expand :: [a] -> [a] -> Int -> [a]
expand ys xs n =
  let xs' = splitAt n xs in
    fst xs' ++ ys ++ (tail . snd $ xs')

apply :: Rule -> Phrase -> [Phrase] 
apply (Rule r) (Phrase ph) =
  do
    n <- whenIs (==hd) ph
    let ph' = expand body ph n
    return (Phrase ph')
      where (hd, Phrase body) = r

derive :: [Rule] -> Phrase -> [Phrase]
derive rs ph
  | isDone ph = [ph]
  | otherwise = concatMap (flip apply ph) rs

reduce :: [Rule] -> Phrase -> [Phrase]
reduce rs ph
  | isDone ph = [ph]
  | otherwise = concatMap (reduce rs) (derive rs ph)

-- not actually part of formal grammars, but makes my life roughly infinity% easier
-- TODO: move to a different module for extensions to the mathematical formalisms.
limitedReduce :: Int -> [Rule] -> Phrase -> [Phrase]
limitedReduce limit rs ph
  | isDone ph && (size > limit) = []
  | isDone ph                   = [ph]
  | otherwise                   = concatMap (limitedReduce limit rs) (derive rs' ph)
  where size = case ph of Phrase ph' -> length ph'
        rs'  = filter (\r -> size + diff r <= limit) rs

diff :: Rule -> Int
diff (Rule (_,Phrase body)) = count isTerminal body - 1

start :: Symbol
start = NonTerminal "S"

{- --for testing in ghci
a,b :: Symbol
a = Terminal "a"
b = Terminal "b"
r1,r2 :: Rule
r1 = rule (start,Phrase [a,start,b])
r2 = rule (start,Phrase [a,b])
-}

whenIs :: (a -> Bool) -> [a] -> [Int]
whenIs _ []     = []
whenIs p (x:xs) = if p x then 0:rest else rest
  where rest = map (+1) (whenIs p xs)

count :: (a -> Bool) -> [a] -> Int
count p []     = 0
count p (x:xs) = fromBool (p x) + count p xs

fromBool :: Bool -> Int
fromBool True  = 1
fromBool False = 0
