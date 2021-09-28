module Chap30 where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import System.Random (randomRIO)

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if i `elem` [1 .. 9] then throwIO DivideByZero else throwIO StackOverflow

main :: IO ()
main = forever $
  do
    let tryS :: IO () -> IO (Either AsyncException (Either ArithException ()))
        tryS = try . try
    exception <- tryS randomException
    case exception of
      Left UserInterrupt -> putStrLn "BIG interrupt"
      Left _ -> putStrLn "async"
      Right e -> putStrLn "arith"
    putStrLn "Live to loop another day!" -- microseconds
    threadDelay (1 * 100000)