{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Tasty.Stats (statsReporter, consoleStatsReporter) where

import Control.Concurrent.STM (atomically, readTVar, TVar, STM, retry)
import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.IntMap (IntMap)
import Data.List (dropWhileEnd)
import Data.Monoid (Endo(..))
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Data.Time (getCurrentTime, UTCTime, formatTime, defaultTimeLocale)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners
import qualified Data.IntMap as IntMap

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

newtype StatsPath = StatsPath FilePath

instance IsOption (Maybe StatsPath) where
  defaultValue = Nothing
  parseValue = Just . Just . StatsPath
  optionName = Tagged "stats"
  optionHelp = Tagged "A file path to store the collected statistics"

-- | Reporter with support to collect statistics in a file.
statsReporter :: Ingredient
statsReporter = TestReporter optDesc runner
  where optDesc = [ Option (Proxy :: Proxy (Maybe StatsPath)) ]
        runner opts tree = do
          StatsPath path <- lookupOption opts
          pure $ collectStats (getNumThreads $ lookupOption opts) path $ IntMap.fromList $ zip [0..] $ testsNames opts tree

composeReporters :: Ingredient -> Ingredient -> Ingredient
composeReporters (TestReporter o1 f1) (TestReporter o2 f2) =
  TestReporter (o1 ++ o2) $ \o t ->
  case (f1 o t, f2 o t) of
    (g1, Nothing) -> g1
    (Nothing, g2) -> g2
    (Just g1, Just g2) -> Just $ \s -> do
      h1 <- g1 s
      h2 <- g2 s
      pure $ \x -> h1 x >> h2 x
composeReporters _ _ = error "Only TestReporters can be composed"

-- | Console reporter with support to collect statistics in a file.
consoleStatsReporter :: Ingredient
consoleStatsReporter = composeReporters consoleTestReporter statsReporter

zipMap :: IntMap a -> IntMap b -> IntMap (a, b)
zipMap a b = IntMap.mapMaybeWithKey (\k v -> (v,) <$> IntMap.lookup k b) a

waitFinished :: TVar Status -> STM Result
waitFinished = readTVar >=> \case
  Done x -> pure x
  _      -> retry

foldEndo :: (Functor f, Foldable f) => f (a -> a) -> (a -> a)
foldEndo = appEndo . fold . fmap Endo

collectStats :: Int -> FilePath -> IntMap TestName -> StatusMap -> IO (Time -> IO Bool)
collectStats threads path names status = do
  results <- atomically (traverse waitFinished status)
  stats <- mkStat threads >>= pure . flip map (IntMap.toList $ zipMap names results)
  appendFile path $ foldEndo (map showStat stats) ""
  pure (const (pure (and $ fmap resultSuccessful results)))

git :: [String] -> IO String
git args = readProcessWithExitCode "git" args "" >>=
  pure . \case (ExitSuccess, out, _) -> dropWhileEnd isSpace out
               (ExitFailure{}, _, _) -> "Unknown"

getFailInfo :: FailureReason -> Maybe String
getFailInfo TestFailed             = Nothing
getFailInfo (TestTimedOut i)       = Just $ show i
getFailInfo (TestThrewException e) = Just $ show e

showStat :: Stat -> ShowS
showStat Stat{..}
  = idx
  ! name
  ! time
  ! formatTime defaultTimeLocale "%FT%T%QZ" date
  ! success
  ! fromMaybe "" failReason
  ! fromMaybe "" failInfo
  ! desc
  ! shortDesc
  ! gitCommit
  ! gitTag
  ! gitDate
  ! (show numThreads ++) . ('\n':)
  where s ! f = (show s ++) . (';':) . f
        infixr 9 !

mkStat :: Int -> IO ((Int, (TestName, Result)) -> Stat)
mkStat numThreads = do
  gitTag    <- git ["describe", "--dirty", "--tags", "--all"]
  gitCommit <- git ["rev-parse", "HEAD"]
  gitDate   <- git ["log", "HEAD", "-1", "--format=%cd"]
  date      <- getCurrentTime
  pure $ \(idx, (name, r@Result { resultDescription=desc
                                , resultShortDescription=shortDesc
                                , resultTime=time
                                , .. }))
         -> let (failReason, failInfo) = case resultOutcome of
                  Success   -> (Nothing, Nothing)
                  Failure f -> (Just $ show f, getFailInfo f)
            in Stat { success=resultSuccessful r, .. }
