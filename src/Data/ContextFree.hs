module Data.ContextFree
  ( Symbol(Terminal, NonTerminal)
  , Phrase(Phrase)
  , Rule
  , rule
  , isTerminal
  , apply
  , derive
  , reduce
  ) where

data Symbol = Terminal String | NonTerminal String deriving Eq
newtype Phrase = Phrase { symbols :: [Symbol] } deriving Eq
newtype Rule  = Rule (Symbol,Phrase) deriving (Eq,Show)

instance Show Symbol where
  show (Terminal s) = s
  show (NonTerminal s) = s

instance Show Phrase where
  show = concat . map show . symbols

rule :: (Symbol,Phrase) -> Rule
rule specs
  | isValidRule specs = Rule specs
  | otherwise         = error "Invalid rule specification."

isValidRule :: (Symbol,Phrase) -> Bool
isValidRule (hd,_) = not . isTerminal $ hd

isTerminal :: Symbol -> Bool
isTerminal (Terminal _) = True
isTerminal _            = False

isDone :: Phrase -> Bool
isDone = all isTerminal . symbols


--replaces the nth element of xs with ys; expand [30,20,10] [0..10] 7 ==> [0,1,2,3,4,5,30,20,10,8,9,10]
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
  | otherwise = concat . map (flip apply ph) $ rs

reduce :: [Rule] -> Phrase -> [Phrase]
reduce rs ph
  | isDone ph = [ph]
  | otherwise = concat . map (reduce rs) $ (derive rs ph)

{- --for testing in ghci
start,a,b :: Symbol
a = Terminal "a"
b = Terminal "b"
start = NonTerminal "S"
r1,r2 :: Rule
r1 = rule (start,Phrase [a,start,b])
r2 = rule (start,Phrase [a,b])
-}

whenIs :: (a -> Bool) -> [a] -> [Int]
whenIs _ []     = []
whenIs p (x:xs) = if p x then 0:rest else rest
  where rest = map (+1) (whenIs p xs)
