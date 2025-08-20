{-
 - Copyright 2025 Nicholas Martin. All rights reserved.
 - 
 - Redistribution and use in source and binary forms, with or without modification, are permitted
 - provided that the following conditions are met:
 -    1. Redistributions of source code must retain the above copyright notice, this list of conditions
 - and the following disclaimer.
 -    2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
 - and the following disclaimer in the documentation and/or other materials provided with the distribution.
 -    3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse
 - or promote products derived from this software without specific prior written permission.
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 - WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OR MERCHANTABILITY AND FITNESS FOR A
 - PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 - FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXAMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 - LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 - INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 - TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 - ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}

{-
 - This program is designed to sort USD totals into USD denominations
 -}

-- Recursively create counts for each constituent denomination, then build a list from that count
sort :: Integer -> [Integer] -> [Integer]
sort total [x] = total `div` x : []
sort total (x:xs) = counts : sort (total - (counts * x)) xs
    where counts = total `div` x

-- Improve readability, especially for the user, with a list of tuples
denoms :: [(String,Double)]
denoms = [("Hundred",   100.00), 
          ("Fifty",      50.00), 
          ("Twenty",     20.00),
          ("Ten",        10.00),
          ("Five",        5.00), 
          ("One",         1.00), 
          ("Quarter",     0.25),
          ("Dime",        0.10), 
          ("Nickel",      0.05), 
          ("Penny",       0.01)]

-- Prompt user for input total, then count the total into denominations and return the denomination counts
main :: IO ()
main = do
    putStrLn "Enter a total to be sorted: "
    total <- getLine
    let totalCents = round $ (read total) * 100.0
    let denomsCents = map (round . (100*) . snd) denoms
    let counts = sort totalCents denomsCents
    print $ zip (map fst denoms) counts
