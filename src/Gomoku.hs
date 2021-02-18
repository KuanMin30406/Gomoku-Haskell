module Gomoku where

-- To run it, try:
-- ghci
-- :load Gomoku

------- Definitions -------

data Tile = O
          | W 
          | B
        deriving (Ord, Eq, Show)

-- Utility function for play
reverseTile :: Tile -> Tile
reverseTile B = W
reverseTile W = B

-- Size of the board
boardHeight :: Int
boardHeight = 15
boardWidth  :: Int
boardWidth = 15

-- 15 by 15 square board 
initBoard :: [[Tile]]
initBoard = [[O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
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
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O],
             [O, O, O, O, O, O, O, O, O, O, O, O, O, O, O]]

-- Data and types to be used for game logic
data State = State CurrentState [Action]
         deriving (Ord, Eq, Show)

-- Result represent the new state which can be defined as game finished or not
data Result = EndOfGame Double State
            | ContinueGame State
         deriving (Eq, Show)

-- Game will take an action and a state and produce the resulting new state
type Game = Tile -> Action -> State -> Result

-- Player will take the current state and perform an action
type Player = State -> Action

-- Action is a coordinate the agent chooses
data Action = Action (Int, Int)
         deriving (Ord, Eq)

-- CurrentState keeps track of the current board
type CurrentState = [[Tile]]

instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]
    
-- Starting state is empty board and the actions are all possible coordinates in the board
start_state = State initBoard [Action (x,y) | (x,y) <- [(a,b) | a <- [0..14], b <- [0..14]]]

------- Print utility -------

-- Helper for printing, convert type to character
spottostr :: Tile -> String 
spottostr O = "O"
spottostr W = "W"
spottostr B = "B"

-- Helper for printing, convert 2d array to list of string
boardtostr :: [[Tile]] -> [[String]]
boardtostr (h:t) = map (map spottostr) t

-- Helper for printing, print the each row of the board
printline [] =   
  do
      putStr ""
printline (h:t) =
  do
      putStr (h ++ " ")
      printline t

-- Helper for printing, print the board representation in correct format
printboard :: [[String]] -> IO ()
printboard [] =   
  do
      putStrLn ""
printboard (h:t) =
  do
      printline h
      putStrLn ""
      printboard t

-- Print the current game
printGame :: [[Tile]] -> IO ()
printGame brd = 
  do
      putStrLn "Current Board:"
      printboard (boardtostr brd)

------- Game logic -------

-- Updates the game and return result 
gomoku :: Game
gomoku color (Action (x, y)) (State dlst available) 
    | win color (Action (x, y)) dlst          = EndOfGame 1  new_state   -- agent wins
    | available == [(Action (x, y))]          = EndOfGame 0  new_state   -- no more moves, tie
    | otherwise                               = ContinueGame new_state
        where new_state = (State (insert2D color y x dlst) [act | act <- available, act /= (Action (x, y))])

--             N,      NE,     E,     SE,    S,     SW,     W,      NW
directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)]
-- Checks if the selected action results in a win for the game 
-- Need to check if the current selected action will result in 5 tiles of the same color connected in any 8 directions
win :: Tile -> Action -> [[Tile]] -> Bool
win color (Action (x, y)) dlst = or [checkconnected color 4 (a, b) (x, y) dlst | (a, b) <- directions]

-- Check if the direction specified has n in a row of the same color tile 
-- a and b will represent the direction we are currently checking
checkconnected :: Tile -> Int -> (Int, Int) -> (Int, Int) -> [[Tile]] -> Bool
checkconnected _ 0 _ _ _ = True
checkconnected color n (a, b) (x, y) dlst = samecolor color (x+a, y+b) dlst && checkconnected color (n-1) (a, b) (x+a, y+b) dlst

-- Checks if the the (x,y) position in the board is the same as color, if (x,y) is out of bounds then false
samecolor :: Tile -> (Int, Int) -> [[Tile]] -> Bool
samecolor color (x, y) dlst = x < boardWidth && y < boardHeight && x >= 0 && y >= 0 && ((dlst !! y) !! x) == color

-- Two functions taken from https://stackoverflow.com/a/53838631
-- Insert into a list
insert1D :: a -> Int -> [a] -> [a]
insert1D x' 0 (_:xs) = x':xs
insert1D x' p (x:xs) = x : insert1D x' (p - 1) xs

-- Insert into a 2d list
insert2D :: a -> Int -> Int -> [[a]] -> [[a]]
insert2D x'  0 py (r:rs) = insert1D x' py r : rs
insert2D x' px py (r:rs) = r : insert2D x' (px - 1) py rs

------- Simple AI Player -------

-- Really bad AI, just choose the first coordinate available
simple_player :: Player
simple_player (State _ avail) = head avail
