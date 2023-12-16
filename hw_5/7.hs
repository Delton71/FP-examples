import GBM (geomBMIO)
import Data.Foldable
import Text.Printf
import System.Random


-- -----/ Briefcase /-----

decision :: (Fractional a) => [(a, a)] -> a
decision actions = fst (decision' actions)
  where decision' = foldl' (\(r, i) (p, d) -> (r + (d - i) * p, d)) (0, 0)
{-
(r, i): result and indicator.
* "result"     means the final profit after the buying and selling;
* "indicator"  means the status of stocks in the portfolio:
    0 - not in the briefcase,
    1 - otherwise.

(p, d): price and decision.
* "price"     means the current price per stock;
* "decision"  means our decision about buying or selling a stock.
Decision generates "randomly" in briefcase function.


"\(r, i) (p, d) -> (r + (d - i) * p, d)"
How does this work?

1) New indicator is current decision.
2) What about new price?

"(d - i)"
d \ i  0   1
0      0  -1
1      1   0
New result = current result `plus`/`minus` current price
If decision is 0  --> `nothing`/`minus` depends on indicator
    i: (0 / 1) like 
       (do nothing, the briefcase is already empty / 
            buy the stock)
If decision is 1  --> `plus`/`nothing` depends on indicator
    i: (0 / 1) like 
       (sell the stock / 
            do nothing, the stock is already in the briefcase)

[..., (price_i, decision_i), ...] ---->
---> foldl' ---> does all the work.
-}


briefcase :: (Fractional a, Show a) => [a] -> IO a
briefcase prices = do
  gen <- newStdGen
  let decsns' = (randomRs (0, 1) :: StdGen -> [Int]) gen
      decsns = take (length prices - 1) decsns' ++ [0]
            -- "++ [0]": always sell at the last moment
      actions = zip prices (map fromIntegral decsns)
  let res = decision actions
  return res

simulate :: Int -> IO Float
simulate n = do
  prices <- take n <$> geomBMIO 100 0.0 0.2 (1.0/252)
  briefcase prices

avg :: (Foldable t, Fractional a) => t a -> a
avg xs = sum xs / fromIntegral (length xs)

testSimulation :: Int -> Int -> IO Float
testSimulation probe price_len = do
  briefcases <- sequence . replicate probe $ simulate price_len
  return $ avg briefcases


-- -----/ Main /-----

main :: IO ()
main = printf "probes prices_len avg_income\n" >> 
  forM_ ((,) <$> [10, 100, 1000] <*> [1000, 10000, 100000]) (
    \(p, n) -> printf "%6d %10d %f\n" p n =<< testSimulation p n)

-- Examples
{-
* "probes":      the number of experiments performed;
* "prices_len":  the length of the segment with stock prices;
* "avg_income":  the average of the received income.

probes prices_len avg_income
    10       1000 11.958434
    10      10000 21.913773
    10     100000 92.11999
   100       1000 2.6522486
   100      10000 2.143913
   100     100000 21.719854
  1000       1000 -0.0027107506
  1000      10000 1.6466407
  1000     100000 152.07729
-}

{-
The experiment showed that the strategy is more effective over time. 
Although, perhaps it's just that the "stars matched" :)
-}
