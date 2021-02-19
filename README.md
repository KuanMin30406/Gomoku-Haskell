# Gomoku-Haskell
Gomoku game made with Haskell. The game is very similar to Connect Four but more complex, in order to win the game you need to connect 5 pieces together in a Go board and instead of dropping a piece down a column each player takes runs placing a piece on the board. 

# Project Tasks
- Build the go board representation/utility functions
- Implement the game logic code
- Add UI to interact with the game

# Bug backlog
- (Fixed) ~~When simple AI makes a move and the board gets printed, the update is delayed (The tile placed will not appear until the next move)~~
- Win function doesn't always declare a win (Kuan)

# Extra
- Local play (Kuan)
- Add save/load functionality (Make sure to record whos turn was it and if the game was pvp or pve, black tile is always the player who goes first) (Maria)
- Add player vs computer with a competent AI algorithm (Jason)
  - A solution to think about, AI should keep a priority list to check where to place the tile and play defensively (Greedy): 
  - 4 alive > 4 dead > 3 alive ... > place tile in the current longest chain for AI
  - If the AI moves first, can always start off the game by placing a tile in the middle at (7,7) 
- Add GUI
- Add networking

# Development Notes
- When writing the code try to add a comment on what the code is doing
- When writing a function try and add the type declaration
- When pushing up the code into main branch, write the code in a separate branch and create a pull request instead (For possible code review before merge)
