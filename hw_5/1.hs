import System.IO
import Control.Monad

readFloats :: Int -> IO [Float]
readFloats 0 = return []
readFloats n = do num <- getLine
                  let float_num = read num :: Float
                  others <- readFloats (n - 1)
                  return (float_num : others)

{-
-- version with no errors while parsing input
import Text.Read

readFloats :: Int -> IO [Float]
readFloats 0 = return []
readFloats n = do num <- getLine
                  let float_num = readMaybe num :: Maybe Float
                  others <- readFloats (n - 1)
                  case float_num of
                    Nothing -> return (0 : others) -- error "catch no float number!"
                    Just x  -> return (x : others)
-}

askOptions :: [Float] -> IO Float
askOptions floats = do
  putStrLn "1 -\tthe sum     of the numbers;"
  putStrLn "2 -\tthe product of the numbers;"
  putStrLn "Enter option:"
  option <- getLine
  return $ case (read option :: Int) of
              1 -> sum floats
              2 -> product floats
              _ -> error "bad option :("

askFloats :: IO()
askFloats = do 
  putStrLn "Enter the amount of numbers:"
  n <- getLine
  when ((read n :: Int) <= 0) $ error "can't be less than 1 :("
  putStrLn ("Enter " ++ n ++ " numbers:")
  floats <- readFloats (read n :: Int)
  print floats
  result <- askOptions floats
  print result

main :: IO()
main = askFloats


-- Examples
{-
*Main> main
Enter the amount of numbers:
4
Enter 4 numbers:
2.5
4
2
1.0
[2.5,4.0,2.0,1.0]
1 -     the sum     of the numbers;
2 -     the product of the numbers;
Enter option:
2
20.0
-}
