{-# LANGUAGE RecordWildCards, PatternSynonyms #-}

module Assets where


data Option a = Some a | None
    deriving Eq

data Board = Board { a1 :: Option Value , a2 :: Option Value , a3 :: Option Value
                   , b1 :: Option Value , b2 :: Option Value , b3 :: Option Value
                   , c1 :: Option Value , c2 :: Option Value , c3 :: Option Value
                   , turn :: Value }
    deriving Eq

data Value = O | X 
  deriving (Show, Eq)

data Move = A1 | A2 | A3
          | B1 | B2 | B3
          | C1 | C2 | C3
  deriving (Enum, Show, Read, Eq)

instance Bounded Move where
  minBound = A1
  maxBound = C3

instance (Show a) => Show (Option a) where
  show (Some v) = show v
  show  None    = " "

instance Enum Value where
  succ     O = X
  succ     X = O

  fromEnum O = 0
  fromEnum X = 1

  toEnum   0 = O
  toEnum   1 = X

instance Show Board where
  show Board{..} = unlines [sep, row1, sep, row2, sep, row3, sep]
    where sep = " ----------- "
          row1 = unwords ["|", show a1, "|", show a2, "|", show a3, "|"] 
          row2 = unwords ["|", show b1, "|", show b2, "|", show b3, "|"] 
          row3 = unwords ["|", show c1, "|", show c2, "|", show c3, "|"] 


emptyBoard :: Board
emptyBoard = Board e e e  e e e  e e e  X
  where e = None