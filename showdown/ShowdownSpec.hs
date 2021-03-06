{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ShowdownSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.MultiSet as MS
import Control.Monad.State
import Control.Monad.Random
import Data.Map (toList)
import Showdown hiding (main)

testGen :: StdGen
testGen = mkStdGen 123

main :: IO ()
main = hspec $ do
  describe "Showdown.runShowdownStrategies" $ do
    it "two consecutive random strategy runs should have same results" $ do
      let twoRandomShowdowns :: (Monad m) => WithShowdownHistory RPS (Outcome, Outcome) m
          twoRandomShowdowns = do
            one <- runShowdownStrategies randomStrategy randomStrategy
            two <- runShowdownStrategies randomStrategy randomStrategy
            return (one, two)
      result1 <- runStateT (evalRandT twoRandomShowdowns testGen) []
      result2 <- runStateT (evalRandT twoRandomShowdowns testGen) []
      result1 `shouldBe` ((Loss,Loss),[(Paper,Scissors),(Scissors,Rock)])
      result1 `shouldBe`result2

  describe "Showdown.runTestShowdownStrategies" $ do
    it "mimic strategies should choose oppenent's last move" $ do
      (outcomes, history) <-
        runTestShowdownStrategies 4 testGen mimicStrategy mimicStrategy
      outcomes `shouldBe` Map.fromList [(Win,2),(Loss,2)]
      history `shouldBe` [(Rock,Scissors),(Scissors,Rock),(Rock,Scissors),(Scissors,Rock)]

    it "random strategies should choose same move" $ do
      (outcomes, history) <-
        runTestShowdownStrategies 4 testGen randomStrategy randomStrategy
      outcomes `shouldBe` Map.fromList [(Win,1),(Loss,3)]
      history `shouldBe` [(Paper,Rock),(Paper,Scissors),(Paper,Scissors),(Scissors,Rock)]

    it "should respect the howMany parameter" $ property $
      \(Positive n) -> do
        (outcomes, history) <-
          runTestShowdownStrategies n (mkStdGen n)
            (randomStrategy::RpsStrategy) randomStrategy
--        print $ (show n) ++ ": " ++ (show outcomes)
        (length history) `shouldBe` n
        outcomes `shouldBe` (toStats history)
        where
          toStats :: (Showdown a) => ShowdownHistory a -> Stats Outcome
          toStats = MS.toMap . MS.fromList . (map $ uncurry showdown)
