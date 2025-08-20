{-
 - This program is designed to sort USD totals into USD denominations
 -}

-- Recursively create counts for each constituent denomination, then build a list from that count
sort :: Integer -> [Integer] -> [Integer]
sort total [x] = total `div` x : []
sort total (x:xs) = counts : sort (total - (counts * x)) xs
    where counts = total `div` x

denoms :: [Double]
denoms = [100.0, 50.0, 20.0, 10.0, 5.0, 1.0, 0.25, 0.10, 0.05, 0.01]

-- Prompt user for input total, then count the total into denominations and return the denomination counts
main :: IO ()
main = do
    putStrLn "Enter a total to be sorted: "
    total <- getLine
    let totalCents = round $ (read total) * 100.0
    let denomsCents = map (round . (100*)) denoms
    let counts = sort totalCents denomsCents
    print $ zip denoms counts
