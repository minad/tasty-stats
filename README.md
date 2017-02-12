# tasty-stats: Collect statistics of your Tasty test suite

[![Hackage](https://img.shields.io/hackage/v/tasty-stats.svg)](https://hackage.haskell.org/package/tasty-stats)
[![Build Status](https://secure.travis-ci.org/minad/tasty-stats.png?branch=master)](http://travis-ci.org/minad/tasty-stats)

This package provides auto discovery for the tasty test framework.

Use the `Test.Tasty.Stats.consoleStatsReporter` ingredient:

``` haskell
main = defaultMainWithIngredients (Test.Tasty.Stats.consoleStatsReporter : defaultIngredients) testTree
```

Then you can pass the option `--stats stats.csv` to the testsuite.

Currently the following information is collected in this order:

``` haskell
data Stat = Stat
  { idx         :: Int
  , name        :: TestName
  , time        :: Time
  , date        :: UTCTime
  , success     :: Bool
  , failReason  :: Maybe String
  , failInfo    :: Maybe String
  , desc        :: String
  , shortDesc   :: String
  , gitCommit   :: String
  , gitTag      :: String
  , gitDate     :: String
  , numThreads  :: Int
  }
```

With tasty-auto:
``` haskell
-- test/test.hs
{-# OPTIONS_GHC -F -pgmF tasty-auto -optF Test.Tasty.Stats.consoleStatsReporter #-}
```
