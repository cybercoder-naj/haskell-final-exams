{-# LANGUAGE ExistentialQuantification #-}
module IC.TestSuite (TestGroup, TestCase, testGroup, runTests, (-->)) where

import Control.Exception ( evaluate, SomeException, handle
                         , ErrorCall(..), fromException)
import Control.Monad (forM)
import Data.Functor (($>))
import Data.List (foldl')
import GHC.Stack ( HasCallStack, callStack, getCallStack
                 , SrcLoc(srcLocFile, srcLocStartLine) )
import System.Exit (exitFailure, exitSuccess)
import System.Timeout (timeout)

data TestGroup = TestGroup !String ![TestCase]
data TestCase = forall a. (HasCallStack, Eq a, Show a) => TestCase a !a

testGroup :: String -> [TestCase] -> TestGroup
testGroup = TestGroup

runTests :: [TestGroup] -> IO ()
runTests tests = do
  allPass <- fmap and (mapM goTest tests)
  if allPass then exitSuccess
  else exitFailure

goTest :: TestGroup -> IO Bool
goTest (TestGroup name cases) = do
  counts <- forM cases (handle majorExceptionHandler . goTestOne name)
  let passes = filter id counts
  putStrLn $ name ++ ": " ++ show (length passes)
                  ++ " / " ++ show (length counts)
  putStrLn ""
  return $ length passes == length counts
  where
    majorExceptionHandler :: SomeException -> IO Bool
    majorExceptionHandler e =
      putStrLn ("Argument exception: " ++ show e) $> False

-- this is irrefutable to remind us we don't want to evaluate before being under
-- the exception handler!
goTestOne :: [Char] -> TestCase -> IO Bool
goTestOne name (TestCase ~actual expected) = handle exceptionHandler $ do
  r <- timeout (10^6) $ if actual == expected then return True
                        else failedStanza Nothing
  handleTimeout r
  where
    failedStanza :: Maybe SomeException -> IO Bool
    failedStanza e = do
      putStrLn . unlines $
        [ " Failure in " ++ name ++ sourceLoc ++ ":"
        , " expected: " ++ show expected
        , "  but got: " ++ maybe (show actual) clean e]
      return False

    handleTimeout :: Maybe Bool -> IO Bool
    handleTimeout Nothing =
      do putStrLn (" Timeout in " ++ name ++ sourceLoc)
         return False
    handleTimeout (Just r) = return r

    sourceLoc :: HasCallStack => String
    sourceLoc = maybe "" (showSrcLoc . snd) (lastMaybe (getCallStack callStack))
      where showSrcLoc loc =
              " (" ++ srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ ")"

    exceptionHandler :: SomeException -> IO Bool
    exceptionHandler = failedStanza . Just

    clean :: SomeException -> String
    clean e
      | Just (ErrorCallWithLocation msg trace) <- fromException e =
        -- the last line of the trace is the student's code (they won't add
        -- HasCallStack, so only one line will count): this is indented by 2
        -- characters, so trim one and use the other as the space between from.
        msg ++ " (from" ++ tail (last (lines trace)) ++ ")"
      | otherwise = show e

infix 0 -->
(-->) :: (HasCallStack, Eq a, Show a) => a -> a -> TestCase
(-->) = TestCase

lastMaybe :: [a] -> Maybe a
lastMaybe = foldl' (const Just) Nothing
