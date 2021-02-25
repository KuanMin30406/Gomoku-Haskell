module Gomoku where

-- To run it, try:
-- ghci
-- :load Gomoku

------- Definitions -------

data Tile = O
          | W                     -- White tile is always second player
          | B                     -- Black tile is always first player
        deriving (Ord, Eq, Show)

-- Utility function for play
reverseTile :: Tile -> Tile
reverseTile B = W
reverseTile W = B

tiletoPlayer :: Tile -> [Char]
tiletoPlayer B = "Player 1"
tiletoPlayer W = "Player 2"

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
start_state = State initBoard generateCoordinates

------- Print/Save/Load utility -------
-- Helper for printing, modifying unwords to add two spaces instead of one
myunwords :: [String] -> String
myunwords = foldr (\ x y -> x ++ "  " ++ y) ""

-- give ["aa", "bb"] -> "aa  bb"

-- Helper for printing, convert type to character
spottostr :: Tile -> String 
spottostr O = "O"
spottostr W = "W"
spottostr B = "B"

-- Helper for printing, convert 2d array to list of string
boardtostr :: [[Tile]] -> [[String]]
boardtostr (h:t) = map (map spottostr) (h:t)

-- Helper for Loading, convert character to type
strtotile :: String -> Tile
strtotile "O" = O
strtotile "B" = B 
strtotile "W" = W

-- Helper for Loading, convert 2D string array to 2D tile array
strtoboard :: [[String]] -> [[Tile]]
strtoboard lst = map (map strtotile) lst

-- Convert the board to a string that can be printed or written to file
gametostr :: [[Tile]] -> String
gametostr board = unlines (map myunwords (boardtostr board))

-- Print the current game
printGame :: [[Tile]] -> IO ()
printGame brd = 
  do
      putStrLn ""
      putStrLn "Current Board:"
      putStrLn (gametostr brd)

------- Game logic -------

-- Updates the game and return result 
gomoku :: Game
gomoku color (Action (x, y)) (State dlst available) 
    | win color (insert2D color y x dlst)     = EndOfGame 1  new_state   -- agent wins
    | available == [Action (x, y)]            = EndOfGame 0  new_state   -- no more moves, tie
    | otherwise                               = ContinueGame new_state
        where new_state = State (insert2D color y x dlst) [act | act <- available, act /= Action (x, y)]

--             N,      NE,     E,     SE,    S,     SW,     W,      NW
directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)]
-- Checks if the board game has a chain of 5 tiles for the chosen tile
win :: Tile -> [[Tile]] -> Bool
win color dlst = or [check8directions color (x,y) dlst | Action (x,y) <- generateCoordinates, samecolor color (x,y) dlst]

-- Check if the there are 4 more tiles of the same colors connected in any 8 directions
check8directions :: Tile -> (Int, Int) -> [[Tile]] -> Bool
check8directions color (x, y) dlst = or [checkconnected color 4 (a,b) (x,y) dlst | (a,b) <- directions]

-- Check if the direction specified has n in a row of the same color tile 
-- a and b will represent the direction we are currently checking
checkconnected :: Tile -> Int -> (Int, Int) -> (Int, Int) -> [[Tile]] -> Bool
checkconnected _ 0 _ _ _ = True
checkconnected color n (a, b) (x, y) dlst = samecolor color (x+a, y+b) dlst && checkconnected color (n-1) (a,b) (x+a, y+b) dlst

-- Checks if the the (x,y) position in the board is the same as color, if (x,y) is out of bounds then false
samecolor :: Tile -> (Int, Int) -> [[Tile]] -> Bool
samecolor color (x, y) dlst = x < boardWidth && y < boardHeight && x >= 0 && y >= 0 && ((dlst !! y) !! x) == color

------- Utility Functions -------

-- Two functions taken from https://stackoverflow.com/a/53838631
-- Insert into a list
insert1D :: a -> Int -> [a] -> [a]
insert1D x' 0 (_:xs) = x':xs
insert1D x' p (x:xs) = x : insert1D x' (p - 1) xs

-- Insert into a 2d list
insert2D :: a -> Int -> Int -> [[a]] -> [[a]]
insert2D x'  0 py (r:rs) = insert1D x' py r : rs
insert2D x' px py (r:rs) = r : insert2D x' (px - 1) py rs

-- Generate all possible coordinates as actions for the grid
generateCoordinates :: [Action]
generateCoordinates = [Action (x,y) | (x,y) <- [(a,b) | a <- [0..boardWidth-1], b <- [0..boardHeight-1]]]

------- Simple AI Player -------

-- Really bad AI, just choose the first coordinate available
simple_player :: Player
simple_player (State _ avail) = head avail

