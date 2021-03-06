# tasty-stats: Collect statistics of your Tasty test suite in a CSV file

[![Hackage](https://img.shields.io/hackage/v/tasty-stats.svg)](https://hackage.haskell.org/package/tasty-stats)
[![Build Status](https://secure.travis-ci.org/minad/tasty-stats.png?branch=master)](http://travis-ci.org/minad/tasty-stats)

This package is useful to collect statistics of your Tasty test suite in a CSV file. Since
timing information and the git commit is collected, the data can be used to find performance regressions between commits.

Use the `Test.Tasty.Stats.consoleStatsReporter` ingredient:

``` haskell
main = defaultMainWithIngredients (Test.Tasty.Stats.consoleStatsReporter : defaultIngredients) testTree
```

Then you can pass the option `--stats stats.csv` to the testsuite.
Currently the following information is collected in this order:

```
idx, name, time, result, gitdate, gitcommit, date, nthreads, description
```

With tasty-auto:
``` haskell
-- test/test.hs
{-# OPTIONS_GHC -F -pgmF tasty-auto -optF Test.Tasty.Stats.consoleStatsReporter #-}
```
