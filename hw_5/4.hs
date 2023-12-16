import System.Environment
import Control.Monad
import System.Random
import Text.Printf

getRandomTtiplets :: Int -> IO [(Double, Double, Double)]
getRandomTtiplets num = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  gen3 <- newStdGen
  let xs = (randomRs (-1, 1) :: StdGen -> [Double]) gen1
      ys = (randomRs (-1, 1) :: StdGen -> [Double]) gen2
      zs = (randomRs (-1, 1) :: StdGen -> [Double]) gen3
  return (take num (zip3 xs ys zs))

getPropPointsBallInCube :: Int -> IO Double
getPropPointsBallInCube num = do
  points <- getRandomTtiplets num
  let in_ball_count = length . filter (\(a,b,c) -> a*a+b*b+c*c <= 1) $ points
  print (in_ball_count, length points)
  return $ 6 * (fromIntegral in_ball_count / fromIntegral (length points))

{-
main = do args <- getArgs
          when (length args /= 1) $ error "1 argument was expected!"
          let num = read $ head args :: Int
          getPropPointsBallInCube num >>= print
-}

main = forM_ [1, 10, 1000, 100000, 10000000] $ 
  \n -> printf "%10d %f\n" n =<< getPropPointsBallInCube n


-- Examples
{-
*Main> main
(0,1)
         1 0.0
(5,10)
        10 3.0
(515,1000)
      1000 3.09
(52287,100000)
    100000 3.1372199999999997
(5235815,10000000)
  10000000 3.141489
-}
