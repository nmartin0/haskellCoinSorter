{-
 - Copyright 2025 Nicholas Martin
 - Redistribution and use in source and binary forms, with or without modification, are permitted
 - provided that the following conditions are met:
 - 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 - and the following disclaimer.
 - 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
 - and the following disclaimer in the documentation and/or other materials provided with the distribution.
 - 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse
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
