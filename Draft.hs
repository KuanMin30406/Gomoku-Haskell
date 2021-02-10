module Draft where
    data Spot = O
            | P 
            | C
    
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

    spottostr :: Spot -> String 
    spottostr O = "O"
    spottostr P = "P"
    spottostr C = "C"

    boardtostr :: [[Spot]] -> [[String]]
    boardtostr (h:t) = map (map spottostr) t

    holder x = putStr (x ++ " ")
    printbline bline = mapM_ (holder . show) bline
    printboard (h:t) = printbline h : printboard t

    Show (IO ()) = putStrLn ""









