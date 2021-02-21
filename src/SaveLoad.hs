module SaveLoad where

import Gomoku

saveboard board = do
  let file = "Saves/board.txt"
  let saved_board = gametostr board
  writeFile file saved_board

loadboard = do
  let file = "Saves/board.txt"
  contents <- readFile file
  let saved_board = strtoboard (map words (lines contents))
  return saved_board

