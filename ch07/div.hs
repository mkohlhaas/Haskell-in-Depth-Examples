{-# LANGUAGE DeriveAnyClass #-}

import Control.Exception (throw, throwIO)
import Control.Monad.Catch

data MyArithException = DivByZero | OtherArithException
 deriving (Show, Exception)

--------------------
-- Pure functions --
--------------------

divPure :: Int -> Int -> Int
divPure _ 0 = throw DivByZero
divPure a b = a `div` b

mult :: Int -> Int -> Int
mult 0 _ = 0
mult a b = a * b

-----------------------
-- Monadic functions --
-----------------------

divIO :: Int -> Int -> IO Int
divIO _ 0 = throwIO DivByZero
divIO a b = pure (a `div` b)

divM :: MonadThrow m => Int -> Int -> m Int
divM _ 0 = throwM DivByZero
divM a b = pure (a `div` b)

---------------------------
-- Monadic test function --
---------------------------

testComputation :: MonadThrow m => Int -> Int -> Int -> m Int
testComputation a b c = divM a b >>= divM c

-----------------------
-- IO test functions --
-----------------------

divTestWithRecovery ::  Int -> Int -> Int -> IO Int
divTestWithRecovery a b c = try (testComputation a b c) >>= pure . dealWith
-- divTestWithRecovery a b c = dealWith <$> try (testComputation a b c)
  where
    dealWith :: Either MyArithException Int -> Int
    dealWith (Right r) = r
    dealWith (Left _) = 0

divTestWithRecovery2 ::  Int -> Int -> Int -> IO Int
divTestWithRecovery2 a b c = tryJust isDivByZero (testComputation a b c) >>= pure . dealWith
  where
    isDivByZero :: MyArithException -> Maybe ()
    isDivByZero DivByZero = Just ()
    isDivByZero _ = Nothing
    dealWith (Right r) = r
    dealWith (Left _) = 0


divTestIO :: Int -> Int -> Int -> IO Int
divTestIO a b c = testComputation a b c `catch` handler
  where
    handler :: MyArithException -> IO Int
    handler e = do
      putStrLn $ "We've got an exception: " ++ show e ++ "\nUsing default value 0"
      pure 0

----------
-- Main --
----------

main :: IO ()
main = do
  divTestWithRecovery 10 0 2 >>= print   -- 0
  divTestWithRecovery2 10 0 2 >>= print  -- 0
  divTestIO 10 0 2 >>= print             -- We've got an exception: DivByZero. Using default value 0. 0
