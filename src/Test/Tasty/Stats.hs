{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Tasty.Stats (statsReporter, consoleStatsReporter) where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (atomically, readTVar, TVar, STM, retry)
import Control.Monad ((>=>))
import Data.Char (isSpace, isPrint)
import Data.Foldable (fold)
import Data.IntMap (IntMap)
import Data.List (dropWhileEnd, intersperse)
import Data.Monoid (Endo(..))
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners
import qualified Data.IntMap as IntMap

newtype StatsFile = StatsFile FilePath

instance IsOption (Maybe StatsFile) where
  defaultValue = Nothing
  parseValue = Just . Just . StatsFile
  optionName = Tagged "stats"
  optionHelp = Tagged "CSV file to store the collected statistics"

-- | Reporter with support to collect statistics in a file.
statsReporter :: Ingredient
statsReporter = TestReporter optDesc runner
  where optDesc = [ Option (Proxy :: Proxy (Maybe StatsFile)) ]
        runner opts tree = do
          StatsFile file <- lookupOption opts
          pure $ collectStats (getNumThreads $ lookupOption opts) file $ IntMap.fromList $ zip [0..] $ testsNames opts tree

-- | Console reporter with support to collect statistics in a file.
consoleStatsReporter :: Ingredient
consoleStatsReporter = composeReporters consoleTestReporter statsReporter

composeReporters :: Ingredient -> Ingredient -> Ingredient
composeReporters (TestReporter o1 f1) (TestReporter o2 f2) =
  TestReporter (o1 ++ o2) $ \o t ->
  case (f1 o t, f2 o t) of
    (g, Nothing) -> g
    (Nothing, g) -> g
    (Just g1, Just g2) -> Just $ \s -> do
      (h1, h2) <- concurrently (g1 s) (g2 s)
      pure $ \x -> uncurry (&&) <$> concurrently (h1 x) (h2 x)
composeReporters _ _ = error "Only TestReporters can be composed"

zipMap :: IntMap a -> IntMap b -> IntMap (a, b)
zipMap a b = IntMap.mapMaybeWithKey (\k v -> (v,) <$> IntMap.lookup k b) a

waitFinished :: TVar Status -> STM Result
waitFinished = readTVar >=> \case
  Done x -> pure x
  _      -> retry

collectStats :: Int -> FilePath -> IntMap TestName -> StatusMap -> IO (Time -> IO Bool)
collectStats nthreads file names status = do
  results <- atomically (traverse waitFinished status)
  rows    <- resultRow nthreads $ IntMap.toList $ zipMap names results
  exists  <- doesFileExist file
  if exists
    then appendFile file $ formatCSV rows ""
    else writeFile  file $ formatCSV (header : rows) ""
  pure $ const $ pure $ and $ fmap resultSuccessful results

git :: [String] -> IO String
git args = readProcessWithExitCode "git" args "" >>=
  pure . \case (ExitSuccess, out, _) -> dropWhileEnd isSpace out
               (ExitFailure{}, _, _) -> "Unknown"

foldEndo :: (Functor f, Foldable f) => f (a -> a) -> (a -> a)
foldEndo = appEndo . fold . fmap Endo

formatCSV :: [[String]] -> ShowS
formatCSV = foldEndo . map ((. ('\n':)) . foldEndo . intersperse (',':) . map field)
  where field s | all isValid s = (s++)
                | otherwise        = ('"':) . escape s . ('"':)
        escape ('"':s) = ("\\\""++) . escape s
        escape (c:s)   = (c:) . escape s
        escape []      = id
        isValid ' '    = True
        isValid ','    = False
        isValid c      = isPrint c && not (isSpace c)

header :: [String]
header = ["idx", "name", "time", "result", "gitdate", "gitcommit", "date", "nthreads", "description"]

resultRow :: Int -> [(Int, (TestName, Result))] -> IO [[String]]
resultRow nthreads' results = do
  let nthreads = show nthreads'
  gitcommit <- git ["rev-parse", "HEAD"]
  gitdate   <- git ["log", "HEAD", "-1", "--format=%cd"]
  date      <- formatTime defaultTimeLocale "%FT%T%QZ" <$> getCurrentTime
  pure $ flip map results $
    \(show -> idx, (name, Result { resultDescription=dropWhileEnd isSpace -> description
                                 , resultShortDescription=result
                                 , resultTime=show -> time })) ->
    [idx, name, time, result, gitdate, gitcommit, date, nthreads, description]
