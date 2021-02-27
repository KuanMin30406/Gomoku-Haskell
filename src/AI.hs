module AI where

import Gomoku
import Data.List
import Data.Maybe

allPos =   [[(x,0) | x <- [0..14]],
            [(x,1) | x <- [0..14]],
            [(x,2) | x <- [0..14]],
            [(x,3) | x <- [0..14]],
            [(x,4) | x <- [0..14]],
            [(x,5) | x <- [0..14]],
            [(x,6) | x <- [0..14]],
            [(x,7) | x <- [0..14]],
            [(x,8) | x <- [0..14]],
            [(x,9) | x <- [0..14]],
            [(x,10) | x <- [0..14]],
            [(x,11) | x <- [0..14]],
            [(x,12) | x <- [0..14]],
            [(x,13) | x <- [0..14]],
            [(x,14) | x <- [0..14]]]

data Dir = NW 
        | TN 
        | NE
        | TW
        | TE
        | SW
        | TS
        | SE 
        | None
        deriving Eq

-- NOTE: A chain here refers to a sequence of the same tiles in a row with at least one end open i.e. can be futher extended.
--       A chain being alive means it is open on both ends. 

aiPlayer :: Tile -> Player
-- main function for AI player. returns the move the AI will make next
-- if going first -> start in the middle
-- if going second -> place a tile in a position directly around the opponent's
-- general strategy:
    -- for each of our tiles, calculate in which direction is it an end of the longest chain possible (and whether the chain is alive)
    -- calculate similarly for opponents' tile 
    -- if opponent's highest chain is longer or equal to the player's but theirs is alive and the AI's isn't, 
        -- place a tile at an open end of their opponent's longest chain
aiPlayer tile (State board avail) =
    do
        let s = length avail
        if s == (15*15)
            then avail !! (s `div` 2) 
        else if s == (15*15 - 1)
            then do
                let (x,y) = findFirst (reverseTile tile) board
                let legalmoves = legalMoves avail (x,y)
                if not (null legalmoves)
                    then head legalmoves
                    else head avail
        else
            do
                let ((px, py), (pdir, pcount, palive)) = scores tile board avail
                let ((ox, oy), (odir, ocount, oalive)) = scores (reverseTile tile) board avail
                if pcount < ocount || (pcount == ocount && not palive && oalive )
                    then getMove (ox, oy) (odir, ocount, oalive) avail
                    else getMove (px, py) (pdir, pcount, palive) avail 


getMove :: (Int, Int) -> (Dir, Int, c) -> [Action] -> Action
-- helper to get the action from position of place tile, direction, and chain length
getMove (x,y) (dir, count, alive) avail
    | dir == NW                               = Action (x-count, y-count)
    | dir == TN                               = Action (x, y-count)
    | dir == NE                               = Action (x+count, y-count)
    | dir == TW                               = Action (x-count, y)
    | dir == TE                               = Action (x+count, y)
    | dir == SW                               = Action (x-count, y+count)
    | dir == TS                               = Action (x, y+count)
    | dir == SE                               = Action (x+count, y+count)
    | otherwise                               = head avail


findFirst :: Tile -> [[Tile]] -> (Int, Int)
-- helper to find the position of the first tile on the board
findFirst tile board =
    do
        let rows = map (getIdx tile) board
        let boolrows = map (> -1) rows
        let y = getIdx True boolrows
        let x = rows !! y
        (x,y) 


getIdx :: Eq a => a -> [a] -> Int
-- helper to get index of an element in a list
getIdx e lst =
    do
        let i = elemIndex e lst
        case i of
            Just n -> n
            Nothing -> -1
      

legalPos :: Action -> Bool
-- helper to check if a postion is legal i.e. on the board
legalPos (Action (x,y)) =
    x >= 0 && x <= 14 && y >= 0 && y <= 14


legalMove :: [Action] -> Action -> Bool
-- helper to check if a move is legal i.e. can be done
legalMove avail (Action (x,y)) =
    legalPos (Action (x,y)) && (Action (x,y) `elem` avail)


legalMoves :: [Action] -> (Int, Int) -> [Action]
-- returns a list of all neighboring positions of (x,y) that are legal
legalMoves avail (x,y) = 
    do
        let possiblemoves = [Action(x-1,y-1), Action(x,y-1), Action(x+1,y-1), Action(x-1,y), Action(x+1,y), Action(x-1,y+1), Action(x,y+1), Action(x+1,y+1)]
        filter (legalMove avail) possiblemoves  


scores :: Tile -> [[Tile]] -> [Action] -> ((Int, Int), (Dir, Int, Bool))
-- returns the longest (alive if possible) chain on the board with the position of one of its ends,
    -- the direction of the chain from said position, and whether the chain is alive
scores tile board avail =
    do
        let boardWithPos = zipWith zip allPos board
        let oneDBoard = concat boardWithPos
        let bestValues = map (\ ((x,y), t) -> if t == tile then ((x,y), longestChain tile board avail (x,y)) else ((x,y), (None, 0, False))) oneDBoard
        let counts = map (\ ((x,y), (dir, count, alive)) -> count) bestValues
        let maxVals = maximum counts
        let maxes = filter (\ ((x,y), (dir, count, alive)) -> count == maxVals) bestValues
        let aliveMaxes = filter (\ ((x,y), (dir, count, alive)) -> alive) maxes
        if null aliveMaxes
            then head maxes 
            else head aliveMaxes


longestChain :: Tile -> [[Tile]] -> [Action] -> (Int, Int) -> (Dir, Int, Bool)
-- returns the longest chain (alive if possible) that (x,y) is an end of 
longestChain tile board avail (x,y) = 
    maxChain [chainNW tile board avail (x,y),
            chainTN tile board avail (x,y),
            chainNE tile board avail (x,y),
            chainTW tile board avail (x,y),
            chainTE tile board avail (x,y),
            chainSW tile board avail (x,y),
            chainTS tile board avail (x,y),
            chainSE tile board avail (x,y)]


-- these helpers calculate the length of the chain the (x,y) is an end of in each direction (if it is an end for that chain)
chainNW :: Tile -> [[Tile]] -> [Action] -> (Int, Int) -> (Dir, Int, Bool)
chainNW tile board avail (x,y) =
    if x >= 4 && y >= 4 && ((x == 14 && y == 14) || (legalPos (Action(x+1,y+1)) && board !! (y+1) !! (x+1) /= tile))
        then
            do  
                let dir = NW
                let count = countChain tile board (x,y) dir 0
                if count > 0 then (dir, count, isAlive tile board avail (x,y) dir count) else (NW, 0, False)
        else (NW, 0, False)

chainTN :: Tile -> [[Tile]] -> [Action] -> (Int, Int) -> (Dir, Int, Bool)
chainTN tile board avail (x,y) =
    if y >= 4 && (y == 14 || (legalPos (Action(x,y+1)) && board !! (y+1) !! x /= tile))
        then
            do  
                let dir = TN
                let count = countChain tile board (x,y) dir 0
                if count > 0 then (dir, count, isAlive tile board avail (x,y) dir count) else (TN, 0, False)
        else (TN, 0, False)

chainNE :: Tile -> [[Tile]] -> [Action] -> (Int, Int) -> (Dir, Int, Bool)
chainNE tile board avail (x,y) =
    if x <= 10 && y >= 4 && ((x == 0 && y == 14) || (legalPos (Action(x-1,y+1)) && board !! (y+1) !! (x-1) /= tile))
        then
            do  
                let dir = NE
                let count = countChain tile board (x,y) dir 0
                if count > 0 then (dir, count, isAlive tile board avail (x,y) dir count) else (NE, 0, False)
        else (NE, 0, False)

chainTW :: Tile -> [[Tile]] -> [Action] -> (Int, Int) -> (Dir, Int, Bool)
chainTW tile board avail (x,y) =
    if x >= 4 && (x == 14 || (legalPos (Action(x+1,y)) && board !! y !! (x+1) /= tile))
        then
            do  
                let dir = TW
                let count = countChain tile board (x,y) dir 0
                if count > 0 then (dir, count, isAlive tile board avail (x,y) dir count) else (TW, 0, False)
        else (TW, 0, False)

chainTE :: Tile -> [[Tile]] -> [Action] -> (Int, Int) -> (Dir, Int, Bool)
chainTE tile board avail (x,y) =
    if x <= 10 && (x == 0 || (legalPos (Action(x-1,y)) && board !! y !! (x-1) /= tile))
        then
            do  
                let dir = TE
                let count = countChain tile board (x,y) dir 0
                if count > 0 then (dir, count, isAlive tile board avail (x,y) dir count) else (TE, 0, False)
        else (TE, 0, False)

chainSW :: Tile -> [[Tile]] -> [Action] -> (Int, Int) -> (Dir, Int, Bool)
chainSW tile board avail (x,y) =
    if x >= 4 && y <= 10 && ((x == 14 && y == 0) || (legalPos (Action(x+1,y-1)) && board !! (y-1) !! (x+1) /= tile))
        then
            do  
                let dir = SW
                let count = countChain tile board (x,y) dir 0
                if count > 0 then (dir, count, isAlive tile board avail (x,y) dir count) else (SW, 0, False)
        else (SW, 0, False)

chainTS :: Tile -> [[Tile]] -> [Action] -> (Int, Int) -> (Dir, Int, Bool)
chainTS tile board avail (x,y) =
    if y <= 10 && (y == 0 || (legalPos (Action(x,y-1)) && board !! (y-1) !! x /= tile))
        then
            do  
                let dir = TS
                let count = countChain tile board (x,y) dir 0
                if count > 0 then (dir, count, isAlive tile board avail (x,y) dir count) else (TS, 0, False)
        else (TS, 0, False)

chainSE :: Tile -> [[Tile]] -> [Action] -> (Int, Int) -> (Dir, Int, Bool)
chainSE tile board avail (x,y) =
    if x <= 10 && y <= 10 && ((x == 0 && y == 0) || (legalPos (Action(x-1,y-1)) && board !! (y-1) !! (x-1) /= tile))
        then
            do  
                let dir = SE
                let count = countChain tile board (x,y) dir 0
                if count > 0 then (dir, count, isAlive tile board avail (x,y) dir count) else (SE, 0, False)
        else (SE, 0, False)


countChain :: Tile -> [[Tile]] -> (Int,Int) -> Dir -> Int -> Int  
-- count the number of tiles in a row from (x,y) going in the specified direction
countChain tile board (x,y) dir count
  | board !! y !! x == reverseTile tile     = 0
  | count == 4 || (board !! y !! x == O)    = count
  | dir == NW                               = countChain tile board (x-1, y-1) dir (count+1)
  | dir == TN                               = countChain tile board (x, y-1) dir (count+1)
  | dir == NE                               = countChain tile board (x+1, y-1) dir (count+1)
  | dir == TW                               = countChain tile board (x-1, y) dir (count+1)
  | dir == TE                               = countChain tile board (x+1, y) dir (count+1)
  | dir == SW                               = countChain tile board (x-1, y+1) dir (count+1)
  | dir == TS                               = countChain tile board (x, y+1) dir (count+1)
  | dir == SE                               = countChain tile board (x+1, y+1) dir (count+1)
  | otherwise                               = 0


maxChain :: [(Dir, Int, Bool)] -> (Dir, Int, Bool)
-- returns the longest chain (alive if possible) from a list of chains (that starts from the same (x,y) going in the 8 surrounding directions)
maxChain lst =
    do
        let vals = map (\ (dir, count, alive) -> count) lst
        let maxVals = maximum vals
        let maxes = filter (\ (dir, count, alive) -> count == maxVals) lst
        let aliveMaxes = filter (\ (dir, count, alive) -> alive) maxes
        if null aliveMaxes
            then head maxes 
            else head aliveMaxes


isAlive :: Tile -> [[Tile]] -> [Action] -> (Int, Int) -> Dir -> Int -> Bool 
-- check whether a chain is alive
-- isAlive tile board avail (x,y) dir count
--     | dir == NW                               = legalMove avail (Action(x-count,y-count))
--     | dir == TN                               = legalMove avail (Action(x,y-count))
--     | dir == NE                               = legalMove avail (Action(x+count,y-count))
--     | dir == TW                               = legalMove avail (Action(x-count,y))
--     | dir == TE                               = legalMove avail (Action(x+count,y))
--     | dir == SW                               = legalMove avail (Action(x-count,y+count))
--     | dir == TS                               = legalMove avail (Action(x,y+count))
--     | dir == SE                               = legalMove avail (Action(x+count,y+count))
--     | otherwise                               = False
isAlive tile board avail (x,y) dir count
    | dir == NW                               = legalMove avail (Action(x+1,y+1))
    | dir == TN                               = legalMove avail (Action(x,y+1))
    | dir == NE                               = legalMove avail (Action(x-1,y+1))
    | dir == TW                               = legalMove avail (Action(x+1,y))
    | dir == TE                               = legalMove avail (Action(x-1,y))
    | dir == SW                               = legalMove avail (Action(x+1,y-1))
    | dir == TS                               = legalMove avail (Action(x,y-1))
    | dir == SE                               = legalMove avail (Action(x-1,y-1))
    | otherwise                               = False

------- Tests -------

actions = [Action (x,y) | (x,y) <- [(a,b) | a <- [0..14], b <- [0..14], not (a == 2 && b == 3)]]

testboard = [[O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, B, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O]] 

teststate = State testboard actions

testboard2 = [[O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, B, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, B, B, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, B, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O]] 
actions2 = [Action (x,y) | (x,y) <- [(a,b) | a <- [0..14], b <- [0..14], not ((a == 5 && b == 2) || (a == 4 && b == 3) || (a == 5 && b == 3) || (a == 5 && b == 4))]]

testboard3 = [[O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, B, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, B, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, B, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, W, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, B, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, B, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, B, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
              [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O]] 
actions3 = [Action (x,y) | (x,y) <- [(a,b) | a <- [0..14], b <- [0..14], not ((a == 3 && b == 2) || (a == 4 && b == 3) || (a == 5 && b == 4) || (a == 6 && b == 5) ||  (a == 7 && b == 9) ||  (a == 8 && b == 10) ||  (a == 9 && b == 11))]]
test3 = aiPlayer B (State testboard3 actions3)