module Main where

import System.Random.Shuffle

func1 x y = do

    let z = x ++ [head y]

    putStrLn  ""
    putStrLn ("Your cards : " ++ (show z))

    if sum z > 21
    then return (z, tail y)
    else do
        putStrLn ""
        putStrLn "Please select Hit(H) or Stand(Not H)."
        com <- getLine
        case com of
            "H" -> do
                putStrLn "You selected Hit."
                let u = tail y
                func1 z u

            _ -> do
                putStrLn "You selected Stand."
                return (z, tail y)

-- Dealer turn
func2 x y = do
    let z = x ++ [head y]
    let u = tail y

    putStrLn ("Dealers cards : " ++ (show z))
    let ds = sum z 

    putStrLn ""

    if sum z > 21
    then do
        return (z, tail y)
         
    else if ds < 17
         then do
             if elem 1 z == True
             then if ds + 10 < 16
                  then do
                      putStrLn "Dealer: Hit"
                      func2 z u
                  else do
                      putStrLn "Dealer: Stand"
                      return (z, tail y)
             else do
                 putStrLn "Dealer: Hit"
                 func2 z u
         else do
             putStrLn "Dealer: Stand"
             return (z, tail y)
        

main :: IO ()
main = do

    let c = [1 .. 10] ++ [10, 10, 10]
    c2 <- shuffleM c

    let dc = [head c2]
    let c3 = drop 1 c2
    print ("Dealers cards : " ++ (show dc) ++ " [-] ")

    let pc = [head c3]
    let c4 = drop 1 c3

    (pc2, c5) <- func1 pc c4

    putStrLn ""
    putStrLn ("Your cards : " ++ (show pc2))

    putStrLn ""
    putStrLn "Dealers turn"

    (dc2, c6) <- func2 dc c5
    putStrLn ("Dealer cards : " ++ (show dc2))
    putStrLn ("Players cards : " ++ (show pc2))
    if sum pc2 > 21
    then do
        putStrLn "You loose"
    else if sum dc2 > 21
         then putStrLn "Dealer loose"
         else if sum pc2 == sum dc2
              then print "Draw"
              else if sum pc2 > sum dc2
                   then print "You win"
                   else print "Dealer win"

