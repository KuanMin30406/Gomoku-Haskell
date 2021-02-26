module Play where

-- To run it, try:
-- ghci
-- :load Play

import Gomoku
import Text.Read   (readMaybe)
import Control.DeepSeq
import Control.Exception

-- Based on the file Play.hs provided in class

start =
  do
      putStrLn "Welcome to the game Gomoku"
      putStrLn "0=start game, 1=continue game, 2=how to play, 3=exit"
      line <- getLine
      if line == "0"
        then option
        else if line ==  "1"
            then load
        else if line ==  "2"
            then showhelppage
        else if line == "3"
            then putStrLn "Thank you for playing!"
        else start    

option =
  do
      putStrLn "Multiplayer or play against AI? 0=multiplayer, 1=AI, 2=exit"
      line <- getLine
      if line == "0"
        then person_play gomoku B (ContinueGame start_state) simple_player True
        else if line ==  "1"
            then play
        else if line ==  "2"
            then putStrLn "Thank you for playing!"
        else option

play =
  do
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLine
      if line == "0"
        then
            person_play gomoku B (ContinueGame start_state) simple_player False
        else if line ==  "1"
            then computer_play gomoku B (ContinueGame start_state) simple_player
        else if line == "2"
            then putStrLn "Thank you for playing!"
        else play

showhelppage =
    do
      putStrLn ""
      putStrLn "How to play:"
      putStrLn "- On each turn the player will place a tile on an empty square in the 15 by 15 game board"
      putStrLn "- To place a tile, you need to type the coordinate in (x,y) format where x is the column and y is the row"
      putStrLn "- (0,0) starts from top-left and (14,14) is at bottom-right"
      putStrLn "- To win the game you need to form an unbroken chain of 5 tiles or more horizontally, vertically, or diagonally"
      putStrLn ""
      start

person_play :: Game -> Tile -> Result -> Player -> Bool -> IO ()
-- opponent has played, the person must now play
person_play game tile (ContinueGame state) opponent multiplayer =
   do
      let State current avail = state
      printGame current
      if multiplayer then putStrLn (tiletoPlayer tile ++ " choose your move (Type S to save, L to load and E to exit):")
                     else putStrLn "Choose your move (Type S to save, L to load and E to exit):"
      line <- getLine
      if line == "S"
        then do
            save state multiplayer tile
            putStrLn "Current game saved"
            person_play game tile (ContinueGame state) opponent multiplayer
        else if line ==  "L"
            then do
                putStrLn "Loading last-saved game"
                load
        else if line == "E"
            then putStrLn "Thank you for playing!"
        else
            case (readMaybe line :: Maybe Action) of
                Nothing ->
                   person_play game tile (ContinueGame state) opponent multiplayer
                Just action ->
                   if action `elem` avail
                     then
                       if multiplayer then person_play game (reverseTile tile) (game tile action state) opponent multiplayer
                                      else computer_play game (reverseTile tile) (game tile action state) opponent
                     else
                       do
                        putStrLn "Illegal move: There is already a tile placed in that position"
                        person_play game tile (ContinueGame state) opponent multiplayer

person_play game tile (EndOfGame val (State current avail)) opponent multiplayer =
   do
      printGame current
      print_winner game tile (EndOfGame (-val) (State current avail)) opponent multiplayer

computer_play :: Game -> Tile -> Result -> Player -> IO ()
-- person has played, the computer must now play
computer_play game tile (EndOfGame val (State current avail)) opponent =
   do
      printGame current
      print_winner game tile (EndOfGame val (State current avail)) opponent False

computer_play game tile (ContinueGame state) opponent =
   do
      let State current avail = state
      let opponent_move = opponent state
      printGame current
      putStrLn ("The computer chose "++show opponent_move)
      person_play game (reverseTile tile) (game tile opponent_move state) opponent False

print_winner _ tile (EndOfGame val state) _ multiplayer
  | val > 0 = do
      putStrLn "You won!"
      askplay
  | val == 0 = do
      putStrLn "It's a tie!"
      askplay
  | otherwise = do
      if multiplayer then putStrLn (tiletoPlayer (reverseTile tile) ++ " won!")
                     else putStrLn "Computer won!"
      askplay

askplay =
  do
      putStrLn "Play again? 0=yes, 1=no"
      line <- getLine
      if line == "0"
        then start
        else if line ==  "1"
            then putStrLn "Thank you for playing!"
        else askplay

-- Save/Load Utility

-- Helper for load that parses a list of actions
strtoavail :: [String] -> [Action]
strtoavail [] = []
strtoavail (h:t) =  case (readMaybe h :: Maybe Action) of
    Nothing -> strtoavail t
    Just action -> action : strtoavail t

-- Saves the current game to files that can be read later
save :: State -> Bool -> Tile -> IO ()
save state multiplayer tile = do
    let State board avail = state
    let board_file = "Saves/board.txt"
    let meta_file = "Saves/meta.txt"
    let avail_file = "Saves/avail.txt"
    let saved_board = gametostr board
    writeFile board_file saved_board
    writeFile meta_file ((show multiplayer) ++ "\n" ++ (show tile))
    writeFile avail_file (unwords (map show avail))

-- Loads a game that was previously saved 
load = do
    let board_file = "Saves/board.txt"
    let meta_file = "Saves/meta.txt"
    let avail_file = "Saves/avail.txt"
    board_contents <- readFile board_file
    meta_contents <- readFile meta_file
    avail_contents <- readFile avail_file
    evaluate (force board_contents)
    evaluate (force meta_contents)
    evaluate (force avail_contents)
    let saved_board = strtoboard (map words (lines board_contents))
    let (multiplayer:tile:t) = lines meta_contents
    let avail = strtoavail (words avail_contents)
    person_play gomoku (strtotile tile) (ContinueGame (State saved_board avail)) simple_player (multiplayer=="True")



