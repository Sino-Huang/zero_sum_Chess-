-- Code based on framework designed by Steven X. Han from
-- the Australian National University, who has granted
-- permission for the usage of their work by the cohort
-- enrolled in the course COMP1100 in 2017 Semester 2 for
-- education purposes only. No commercial usage is allowed
-- without the explicit permission from the original author. 
--
-- Assignment completed by:
-- Name    :Sukai Huang
-- UID     :u6492211
-- Tutor   :Debashish Chakraborty @u4610248
-- Lab Time:Thursday 3pm

module Battleship where

import Data.List
import Prelude hiding (Left, Right)

type Board = Matrix Cell
type Ships = Matrix Bool

-- showBoard prepares a nice string for printing onto Terminal.
showBoard :: Board -> String
showBoard cells = "+" ++ replicate 21 '-' ++ "+\n"
                    ++ intercalate "\n" (map rowsToGrid cells)
                    ++ "\n+" ++ replicate 21 '-' ++ "+"
    where          
        rowsToGrid :: Row Cell -> String
        rowsToGrid row = "| " ++ intercalate " " (map cStateToCell row) ++ " |"
        
        cStateToCell :: Cell -> String
        cStateToCell cell = case cell of
            Unchecked -> " "
            Hit -> "o"
            Miss -> "x"




data Cell = Unchecked | Hit | Miss
    deriving (Show, Read)
type Matrix a  = [Row a]
type Row a     = [a]

data Condition = Won | Lost | Playing
    deriving (Show)

data Direction  = Up | Down | Left | Right
    deriving (Show, Read)
type Coordinate = (XCoord, YCoord)
type XCoord     = Coord
type YCoord     = Coord
type Coord      = Integer

data ShipType = Carrier | Battleship | Submarine | Cruiser | Destroyer
    deriving (Show, Read, Eq)

data State = State {board        :: Board,
                    ships        :: Ships,
                    condition    :: Condition,
                    numMoves     :: Integer}
                    deriving (Show)
                    
type ShipsOnGrid = [ShipType]
data GenShips = GenShips {gsShips       :: Ships,
                          existingShips :: ShipsOnGrid,
                          finished      :: Bool}
                          deriving (Show)
                    
-- updateList replaces an element by index in a given list.
updateList :: [a] -> Int -> a -> [a]
updateList list n x = take (n) list ++ [x] ++ drop (n + 1) list

validPlacement :: GenShips -> Coordinate -> Direction -> ShipType -> Bool
validPlacement gs c d s = 
                    not (s `elem` (existingShips gs))
                && (all coordInBound onCoords)
                && (not $ any (\x -> isShipAtCoord x (gsShips gs)) $
                         filter coordInBound $ nub $ concatMap getNeighbours onCoords)
    where
        onCoords = getCoords c d (shipLength s)
        
shipLength :: ShipType -> Integer
shipLength shipl = case shipl of
    Carrier -> 5
    Battleship -> 4
    Cruiser -> 3
    Submarine -> 3
    Destroyer -> 2
    
-- getCoords returns the list of Coordinate which would be
-- occupied by the ship.
getCoords :: Coordinate -> Direction -> Integer -> [Coordinate]
getCoords (i, j) dir l = case dir of
    Down  -> map (\x -> (i, x)) [j .. (j + l - 1)]
    Right -> map (\x -> (x, j)) [i .. (i + l - 1)]
    Up    -> map (\x -> (i, x)) [j, (j - 1) .. (j - l + 1)]
    Left  -> map (\x -> (x, j)) [i, (i - 1) .. (i - l + 1)]
    
-- getNeighbours returns a 9-element list containing
-- coordinates around the given coordinate and itself.
getNeighbours :: Coordinate -> [Coordinate]
getNeighbours (x, y) = [(i, j) | i <- [(x - 1) .. (x + 1)], j <- [(y - 1) .. (y + 1)]]
    
coordInBound :: Coordinate -> Bool
coordInBound (x, y)
    | x >= 0 && x <= 9 && y>= 0 && y <= 9 = True
    | otherwise = False

-- isShipAtCoord determines if there is a ship already placed
-- at the coord.
isShipAtCoord :: Coordinate -> Ships -> Bool
isShipAtCoord (x, y) grid = grid !! (fromIntegral y) !! (fromIntegral x)

placeShip :: GenShips -> Coordinate -> Direction -> ShipType -> GenShips
placeShip gen (x,y) direc shipt =  gen{gsShips = case direc of
                                          Down -> case shipt of
                                              Carrier ->  updatesimpledown 5 (gsShips gen) x y
                                              Battleship ->  updatesimpledown 4 (gsShips gen) x y
                                              Cruiser ->  updatesimpledown 3 (gsShips gen) x y
                                              Submarine ->  updatesimpledown 3 (gsShips gen) x y
                                              Destroyer ->  updatesimpledown 2 (gsShips gen)x y
                                          Right ->case shipt of
                                              Carrier ->  updatesimpleright 5 (gsShips gen) x y
                                              Battleship ->  updatesimpleright 4 (gsShips gen) x y
                                              Cruiser ->  updatesimpleright 3 (gsShips gen) x y
                                              Submarine ->  updatesimpleright 3 (gsShips gen) x y
                                              Destroyer ->  updatesimpleright 2 (gsShips gen) x y

                                          Up ->case shipt of
                                              Carrier ->  updatesimpleup 5 (gsShips gen) x y
                                              Battleship ->  updatesimpleup 4 (gsShips gen) x y
                                              Cruiser ->  updatesimpleup 3 (gsShips gen) x y
                                              Submarine ->  updatesimpleup 3 (gsShips gen) x y
                                              Destroyer ->  updatesimpleup 2 (gsShips gen) x y

                                          Left ->case shipt of
                                              Carrier ->  updatesimpleleft 5 (gsShips gen) x y
                                              Battleship ->  updatesimpleleft 4 (gsShips gen) x y
                                              Cruiser ->  updatesimpleleft 3 (gsShips gen) x y
                                              Submarine ->  updatesimpleleft 3 (gsShips gen) x y
                                              Destroyer ->  updatesimpleleft 2 (gsShips gen) x y ,

                                      existingShips = shipt : (existingShips (gen)),
                                      finished = case length(existingShips (gen)) of
                                          5 -> True
                                          _ -> False
}

    where

    updatesimpledown :: Integer -> Ships -> Integer -> Integer  -> Ships
    updatesimpledown length ships coordx coordy
        | length == 1 = updateList ships (fromIntegral coordy) (updateList (ships !! (fromIntegral coordy)) (fromIntegral coordx) True)
        | length > 1  =  updatesimpledown 1 (updatesimpledown (length - 1) ships coordx coordy) coordx (coordy + length - 1)
    updatesimpleup :: Integer -> Ships -> Integer -> Integer  -> Ships
    updatesimpleup length ships coordx coordy
        | length == 1 = updateList ships (fromIntegral coordy) (updateList (ships !! (fromIntegral coordy)) (fromIntegral coordx) True)
        | length > 1  =  updatesimpledown 1 (updatesimpledown (length - 1) ships coordx coordy) coordx (coordy - length + 1)
    updatesimpleleft :: Integer -> Ships -> Integer -> Integer  -> Ships
    updatesimpleleft length ships coordx coordy
        | length == 1 = updateList ships (fromIntegral coordy) (updateList (ships !! (fromIntegral coordy)) (fromIntegral coordx) True)
        | length > 1  =  updatesimpledown 1 (updatesimpledown (length - 1) ships coordx coordy) (coordx-length + 1) coordy
    updatesimpleright :: Integer -> Ships -> Integer -> Integer  -> Ships
    updatesimpleright length ships coordx coordy
        | length == 1 = updateList ships (fromIntegral coordy) (updateList (ships !! (fromIntegral coordy)) (fromIntegral coordx) True)
        | length > 1  =  updatesimpledown 1 (updatesimpledown (length - 1) ships coordx coordy) (coordx+length-1) coordy


transitionState :: State -> Coordinate -> State
transitionState state coord = undefined

testnewships :: Ships
testnewships = replicate 10 (replicate 10 False)

testnewgenship :: GenShips
testnewgenship = GenShips {gsShips = testnewships , existingShips =[], finished =False}

testnewboard :: Board
testnewboard = replicate 10 (replicate 10 Unchecked)

testnewstate :: State
testnewstate = State {board = testnewboard, ships = testnewships, condition = Playing, numMoves = 0 }

getShips :: GenShips -> Ships
getShips GenShips{gsShips = n } = n

getShipType :: GenShips -> [ShipType]
getShipType GenShips {existingShips = n} = n