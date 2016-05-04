{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ShowdownSpec where

import Test.Hspec
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Random
import Showdown hiding (main)

testGen :: StdGen
testGen = mkStdGen 123

main :: IO ()
main = hspec $ do
  describe "Showdown.runShowdownStrategies" $ do
    it "two consecutive random strategy runs should have same results" $ do
      let   twoRandomShowdowns :: (Monad m) => WithShowdownHistory RPS (Outcome, Outcome) m
            twoRandomShowdowns = do
              one <- runShowdownStrategies randomRpsStrategy randomRpsStrategy
              two <- runShowdownStrategies randomRpsStrategy randomRpsStrategy
              return (one, two)
      result1 <- runStateT (evalRandT twoRandomShowdowns testGen) []
      result2 <- runStateT (evalRandT twoRandomShowdowns testGen) []
      result1 `shouldBe` ((Loss,Loss),[(Paper,Scissors),(Scissors,Rock)])
      result1 `shouldBe`result2

  describe "Showdown.runTestShowdownStrategies" $ do
    it "mimic strategies should choose oppenent's last move" $ do
      (outcomes, history) <-
        runTestShowdownStrategies 4 testGen mimicRpsStrategy mimicRpsStrategy
      outcomes `shouldBe` Map.fromList [(Win,2),(Loss,2)]
      history `shouldBe` [(Rock,Scissors),(Scissors,Rock),(Rock,Scissors),(Scissors,Rock)]

    it "random strategies should choose same move" $ do
      (outcomes, history) <-
        runTestShowdownStrategies 4 testGen randomRpsStrategy randomRpsStrategy
      outcomes `shouldBe` Map.fromList [(Win,1),(Loss,3)]
      history `shouldBe` [(Paper,Rock),(Paper,Scissors),(Paper,Scissors),(Scissors,Rock)]
      