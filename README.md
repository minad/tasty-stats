# tasty-stats: Collect statistics of your Tasty test suite

[![Hackage](https://img.shields.io/hackage/v/tasty-stats.svg)](https://hackage.haskell.org/package/tasty-stats)
[![Build Status](https://secure.travis-ci.org/minad/tasty-stats.png?branch=master)](http://travis-ci.org/minad/tasty-stats)

This package provides auto discovery for the tasty test framework.

Use the `Test.Tasty.Stats.consoleReporter` ingredient:

``` haskell
main = defaultMainWithIngredients (Test.Tasty.Stats.consoleReporter : defaultIngredients) testTree
```

With tasty-auto:
``` haskell
-- test/test.hs
{-# OPTIONS_GHC -F -pgmF tasty-auto -optF Test.Tasty.Stats.consoleReporter #-}
```
