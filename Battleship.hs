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


-- | test placeShip function
-- >>> placeShip testGenShips1 (1,2) Down Destroyer
-- GenShips {gsShips = [[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,True,False,False,False,False,False,False,False,False],[False,True,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False]], existingShips = [Destroyer], finished = False}
--
-- >>> placeShip testGenShips1 (10,10) Down Destroyer
-- GenShips {gsShips = [[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False]], existingShips = [], finished = False}
--
--
-- | test transitionState
-- >>> transitionState testState1 (1,1)
-- State {board = [[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Miss,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked]], ships = [[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False]], condition = Playing, numMoves = 1}
--
-- >>> transitionState testState2 (9,1)
-- State {board = [[Hit,Hit,Hit,Hit,Hit,Hit,Hit,Hit,Hit,Hit],[Hit,Hit,Hit,Hit,Hit,Hit,Unchecked,Unchecked,Unchecked,Hit],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked],[Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked,Unchecked]], ships = [[False,False,False,False,False,False,False,False,False,True],[False,False,False,False,False,False,False,False,False,True],[False,False,False,False,False,False,False,False,False,True],[False,False,False,False,True,True,True,False,False,True],[False,False,False,False,False,False,False,False,False,True],[False,True,True,True,False,False,False,False,False,False],[False,False,False,False,False,True,True,True,True,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,True,True]], condition = Won, numMoves = 19}



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
    deriving (Show, Read,Eq)



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


-- placeShip generates a new GenShips data once the the new move is a valid move
-- placeShip use validPlacement to check whether the move is valid or not first.
placeShip :: GenShips -> Coordinate -> Direction -> ShipType -> GenShips
placeShip gen (x,y) direc shipt
    | validPlacement gen (x,y) direc shipt == True  =  gen{gsShips = case direc of
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
                                      finished = if length(existingShips (gen)) >= 4 then True else False}

    | otherwise = gen

    where
-- a group of updatesimple functions are used to update the Ships data in the GenShips data
-- all updatesimple function series uses recursion
    updatesimpledown :: Integer -> Ships -> Integer -> Integer  -> Ships
    updatesimpledown lengthofship shipsmap coordx coordy
        | lengthofship == 1 = updateList shipsmap (fromIntegral coordy) (updateList (shipsmap !! (fromIntegral coordy)) (fromIntegral coordx) True)
        | lengthofship > 1  =  updatesimpledown 1 (updatesimpledown (lengthofship - 1) shipsmap coordx coordy) coordx (coordy + lengthofship - 1)
        | otherwise = error "length is bigger than 1"
    updatesimpleup :: Integer -> Ships -> Integer -> Integer  -> Ships
    updatesimpleup lengthofship shipsmap coordx coordy
        | lengthofship == 1 = updateList shipsmap (fromIntegral coordy) (updateList (shipsmap !! (fromIntegral coordy)) (fromIntegral coordx) True)
        | lengthofship > 1  =  updatesimpleup 1 (updatesimpleup (lengthofship - 1) shipsmap coordx coordy) coordx (coordy - lengthofship + 1)
        | otherwise = error "length is bigger than 1"

    updatesimpleleft :: Integer -> Ships -> Integer -> Integer  -> Ships
    updatesimpleleft lengthofship shipsmap coordx coordy
        | lengthofship == 1 = updateList shipsmap (fromIntegral coordy) (updateList (shipsmap !! (fromIntegral coordy)) (fromIntegral coordx) True)
        | lengthofship > 1  =  updatesimpleleft 1 (updatesimpleleft (lengthofship - 1) shipsmap coordx coordy) (coordx - lengthofship + 1) coordy
        | otherwise = error "length is bigger than 1"

    updatesimpleright :: Integer -> Ships -> Integer -> Integer  -> Ships
    updatesimpleright lengthofship shipsmap coordx coordy
        | lengthofship == 1 = updateList shipsmap (fromIntegral coordy) (updateList (shipsmap !! (fromIntegral coordy)) (fromIntegral coordx) True)
        | lengthofship > 1  =  updatesimpleright 1 (updatesimpleright (lengthofship - 1) shipsmap coordx coordy) (coordx + lengthofship-1) coordy
        | otherwise = error "length is bigger than 1"


-- transitionState update the State data if the move is valid
-- transitionState will check the condition of the game first
transitionState :: State -> Coordinate -> State
transitionState state (x,y)
    | x >= 0 && x <= 9 && y >= 0 && y <= 9 = case condition state of
        Playing
            | (ships state !! (fromIntegral y)) !!(fromIntegral x ) == True -> state{board = (updateList (board state) (fromIntegral y )(updateList ((board state) !! (fromIntegral y)) (fromIntegral x) Hit )),
                                                                  ships = ships state,
                                                                  condition = if length([allboardelement | allboardelement <- ((board state) !! 0 ++ (board state) !! 1 ++ (board state)
                                                                  !! 2 ++ (board state) !! 3 ++ (board state) !! 4 ++ (board state) !! 5 ++ (board state) !! 6 ++ (board state) !! 7 ++
                                                                  (board state) !! 8 ++ (board state) !! 9), allboardelement == Hit]) >= 16 then (if numMoves state < 20 then Won else Lost)
                                                                  else (if numMoves state < 20 then Playing else Lost),

                                                                  numMoves = case (((board state) !! (fromIntegral y)) !!(fromIntegral x )) of
                                                                      Unchecked -> numMoves state
                                                                      _ -> numMoves state + 1 }


            | (ships state !! (fromIntegral y)) !!(fromIntegral x ) == False ->  state{board = (updateList (board state) (fromIntegral y )(updateList ((board state) !! (fromIntegral y)) (fromIntegral x) Miss )),
                                                                  ships = ships state,
                                                                  condition = if numMoves state < 19 then Playing else Lost ,

                                                                  numMoves = numMoves state + 1 }

        _ -> state
    | otherwise = state

-- below are all the test variables for doctest and quickCheck


testShip1 :: Ships
testShip1 = replicate 10 (replicate 10 False)

testShip2 :: Ships
testShip2 = [[False,False,False,False,False,False,False,False,False,True],[False,False,False,False,False,False,False,False,False,True],[False,False,False,False,False,False,False,False,False,True],[False,False,False,False,True,True,True,False,False,True],[False,False,False,False,False,False,False,False,False,True],[False,True,True,True,False,False,False,False,False,False],[False,False,False,False,False,True,True,True,True,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,True,True]]

testBoard1 :: Board
testBoard1 = replicate 10 (replicate 10 Unchecked)

testBoard2 :: Board
testBoard2 = replicate 10 Hit : [Hit,Hit,Hit,Hit,Hit,Hit,Unchecked,Unchecked,Unchecked,Unchecked] : replicate 8 (replicate 10 Unchecked)

testGenShips1 :: GenShips
testGenShips1 = GenShips testShip1 [] False

testGenShips2 :: GenShips
testGenShips2 = GenShips testShip2 [Carrier,Battleship,Cruiser,Submarine,Destroyer] True

testState1 :: State
testState1 = State testBoard1 testShip1 Playing 0

testState2 :: State
testState2 = State testBoard2 testShip2 Playing 19

testState3 :: State
testState3 = State testBoard1 testShip2 Won 18

-- prop_coordInBound can be used for quickCheck to check the property of coordInBound function
prop_coordInBound_ConfirmOut :: Coordinate -> Bool
prop_coordInBound_ConfirmOut (x,y)
    | x >= 0 && x <= 9 && y >= 0 && y <= 9 = coordInBound (x,y)
    | otherwise = not (coordInBound (x,y))





