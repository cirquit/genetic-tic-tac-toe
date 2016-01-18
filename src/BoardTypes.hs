{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}

module BoardTypes where

-- | Board defintions
--
data Board = Board { a1 :: Maybe Value , a2 :: Maybe Value , a3 :: Maybe Value
                   , b1 :: Maybe Value , b2 :: Maybe Value , b3 :: Maybe Value
                   , c1 :: Maybe Value , c2 :: Maybe Value , c3 :: Maybe Value
                   , turn :: Value }

instance Eq Board where
  (Board a1 a2 a3  b1 b2 b3  c1 c2 c3 _) == (Board a1' a2' a3'  b1' b2' b3'  c1' c2' c3' _) = a1 == a1' && a2 == a2' && a3 == a3'
                                                                                           && b1 == b1' && b2 == b2' && b3 == b3'
                                                                                           && c1 == c1' && c2 == c2' && c3 == c3'

instance Show Board where
  show Board{..} = unlines [sturn, sep, row1, sep, row2, sep, row3, sep]
    where sep =   " ----------- "
          sturn = " -- Turn " ++ show (succ turn) ++ " - "
          row1  = unwords ["|", showMV a1, "|", showMV a2, "|", showMV a3, "|"] 
          row2  = unwords ["|", showMV b1, "|", showMV b2, "|", showMV b3, "|"] 
          row3  = unwords ["|", showMV c1, "|", showMV c2, "|", showMV c3, "|"]

          showMV :: Maybe Value -> String
          showMV (Just v) = show v
          showMV _        = " "

-- | Value definitions
--
data Value = O | X 
    deriving (Show, Eq)

instance Enum Value where
  succ     O = X
  succ     X = O

  fromEnum O = 0
  fromEnum X = 1

  toEnum   0 = O
  toEnum   1 = X


-- | Move definitions
--
data Move = A1 | A2 | A3
          | B1 | B2 | B3
          | C1 | C2 | C3
  deriving (Enum, Show, Read, Eq)

instance Bounded Move where
  minBound = A1
  maxBound = C3


-- | Result defintions
--
data Result a = Win a | Tie | Ongoing
  deriving Show
