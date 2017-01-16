{-
 - Test/Base.hs
 -
 - Provides the essential foundation for running automated EOPL3 Test cases.
 -}
module Test.Base where

import           Common.Types

data Test
    = ErrTest  { testName :: Id, testSrc :: Source }
    | NumTest  { testName :: Id, testSrc :: Source, numResult  :: Int }
    | BoolTest { testName :: Id, testSrc :: Source, boolResult :: Bool }
    | ListNumTest  { testName :: String, testSrc :: String, numsResult :: [Int] }
    | ListBoolTest { testName :: String, testSrc :: String, boolsResult :: [Bool] }

data Check
    = FailCheck  { checkName :: Id, checkSrc :: Source }
    | PassCheck  { checkName :: Id, checkSrc :: Source, signature :: String }


