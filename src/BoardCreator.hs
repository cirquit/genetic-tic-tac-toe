{-# LANGUAGE RecordWildCards #-}

module BoardCreator where

import qualified Data.Vector as V
import Data.Hashable
import Data.List
import Data.Map as M (insert, Map(..), empty)

-- import Data.Function
-- import Data.Ord

import BoardLib
import BoardTypes


data BoardRot = BoardRot { boardHash :: Int, rot :: Rotation }
  deriving Show

instance Ord BoardRot where
    compare (BoardRot bhash _) (BoardRot bhash' _) = compare bhash bhash'

instance  Eq BoardRot where
    (BoardRot bhash _) == (BoardRot bhash' _) = bhash == bhash'
    

data Rotation = Rotation { mirrored :: Bool, cw :: Int }
  deriving Show

hashBoard :: Board -> (Board, Int)
hashBoard b@Board{..} = (b, (hash . encodeBoard) b)

    -- vec <- createBoardStatesFrom "lib/tictactoeCombos827.txt"

createMinHashed :: (Board, [(Board, Rotation)]) -> String
createMinHashed (b, bs) = encodeBoard . fst $ minHash
    where hashed = map (\(x,_) -> hashBoard x) bs
          minHash = minimumBy (\(_, x) (_, y) -> compare x y) hashed


encodeBoard :: Board -> String
encodeBoard Board{..} = boardString
    where
        boardString = map showTurn [a1, a2, a3, b1, b2, b3, c1, c2, c3]

        showTurn (Just X) = 'X'
        showTurn (Just O) = 'O'
        showTurn Nothing  = '_'


saveBestHashes :: IO ()
saveBestHashes = do
    vec <- createBoardStatesFrom "lib/tictactoeCombos827.txt"
    let l = map (createMinHashed . createRotations) (V.toList vec)
    writeFile "lib/testinCombos827.txt" (unlines l)



createHashMap :: IO (Map Int (Int, Rotation))
createHashMap = do
        vec <- createBoardStatesFrom "lib/testinCombos827.txt"
        let l = V.toList vec
            (_, boardRots) = unzip (map createRotations l)
            enumBoardRots  = zip [0..] boardRots

        return $ go enumBoardRots M.empty
    where
        go :: [(Int, [(Board, Rotation)])] -> Map Int (Int, Rotation) -> Map Int (Int, Rotation)
        go []            m = m
        go ((i, brs):xs) m = go xs m'
            where m' = foldl' insertBoardRotations m brs

                  insertBoardRotations :: Map Int (Int, Rotation) -> (Board, Rotation) -> Map Int (Int, Rotation)
                  insertBoardRotations m (board, rot) = M.insert bhash (i, rot) m
                      where (_, bhash) = hashBoard board



createRotations :: Board -> (Board, [(Board, Rotation)])
createRotations b = (b, [b0, b1, b2, b3, b4, b5, b6, b7])
    where
        b0 = (          b,     Rotation False 0)
        b1 = (cw (fst b0),     Rotation False 1)
        b2 = (cw (fst b1),     Rotation False 2)
        b3 = (cw (fst b2),     Rotation False 3)
        b4 = (mirror (fst b0), Rotation True  0)
        b5 = (cw (fst b4),     Rotation True  1)
        b6 = (cw (fst b5),     Rotation True  2)
        b7 = (cw (fst b6),     Rotation True  3)

        cw :: Board -> Board
        cw b@Board{..} = b { a1 = c1, a2 = b1, a3 = a1
                           , b1 = c2         , b3 = a2
                           , c1 = c3, c2 = b3, c3 = a3 }
        
        mirror :: Board -> Board
        mirror b@Board{..} = b { a1 = a3         , a3 = a1
                               , b1 = b3         , b3 = b1
                               , c1 = c3         , c3 = c1 }
