{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Showdown where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Random
import qualified Data.Map as M
import qualified Data.MultiSet as MS
import qualified Data.MarkovChain as Markov
import Data.Tuple (swap)
--import Test.QuickCheck
--import Test.Tasty.HUnit
--import Test.Tasty.QuickCheck
--import Test.HUnit

data Outcome = Win | Draw | Loss deriving (Eq, Enum, Ord, Show)

class (Eq a) => Showdown a where
  beats :: a -> a
  showdown :: a -> a -> Outcome
  showdown x y
    | x == y = Draw
    | beats x == y = Win
    | otherwise = Loss

class ShowChar a where showChar :: a -> Char
class ReadChar a where readChar :: Char -> a

data RPS = Rock | Paper | Scissors deriving (Eq, Enum, Bounded, Ord, Show)

instance Showdown RPS where
  beats Rock = Scissors
  beats Paper = Rock
  beats Scissors = Paper

instance ShowChar RPS where
  showChar Rock = 'r'
  showChar Paper = 'p'
  showChar Scissors = 's'

instance ReadChar RPS where
  readChar 'r' = Rock
  readChar 'p' = Paper
  readChar 's' = Scissors
  readChar x = error $ "readChar::RPS - unknown char: " ++ [x]

type ShowdownHistory a = [(a,a)]

type WithShowdownHistory a b m =
  (Ord a, RandomGen g, Monad m) => RandT g (StateT (ShowdownHistory a) m) b

newtype ShowdownStrategy a m =
-- TODO make play :: ShowdownHistory a a -> Rand a ?!
  ShowdownStrategy { play :: WithShowdownHistory a a m }

type RpsStrategy = (Monad m) => ShowdownStrategy RPS m

instance Random RPS where
  randomR (a,b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)

randomBoundedEnum :: (Bounded a, Enum a, Random a, MonadRandom m) => m a
randomBoundedEnum = getRandomR (minBound, maxBound)

randomStrategy :: (Bounded a, Enum a, Random a, Monad m) => ShowdownStrategy a m
randomStrategy = ShowdownStrategy randomBoundedEnum

showdownStrategy :: (Ord a, Bounded a, Enum a, Random a, Monad m) =>
    (ShowdownHistory a -> a) -> ShowdownStrategy a m
showdownStrategy f = ShowdownStrategy $ do
  history <- get
  if null history then randomBoundedEnum
  else return $ f history

cyclicStrategy :: (Ord a, Bounded a, Enum a, Random a, Monad m) =>  ShowdownStrategy a m
cyclicStrategy = showdownStrategy $ next' . fst . head
  where
    next' x = if x == maxBound then minBound else succ x

mimicStrategy :: (Ord a, Bounded a, Enum a, Random a, Monad m) => ShowdownStrategy a m
mimicStrategy = showdownStrategy $ snd . head

userInputStrategy :: (ReadChar a) => ShowdownStrategy a IO
userInputStrategy = ShowdownStrategy $ do
  liftIO $ putStr "Choose (r)ock, (p)aper or (s)cissors: "
  input <- liftIO getLine
  return $ readChar (head input)

newtype RunMarkovRandomWalk a =
  RunMarkovRandomWalk { runRandomWalk :: (Ord a) => Int -> [a] -> Int-> [a] }

instance Random (RunMarkovRandomWalk a) where
  randomR (_,_) _ = undefined
  random gen = (RunMarkovRandomWalk rw, gen')
    where
      rw size trainingSeq start = Markov.run size trainingSeq start gen
      (_, gen') = split gen

markovChainStrategy :: (Ord a, Showdown a, Random a, Monad m) => ShowdownStrategy a m
markovChainStrategy = ShowdownStrategy $ do
  history <- get
  trainingSeq <-
        if null history then randomSample
        else return $ take 10 history
  markovRandomWalk <- getRandom
  let markovChain = (runRandomWalk markovRandomWalk) 100 trainingSeq 0
      guess = markovGuess markovChain
  return guess
  where
    randomSample :: (Random a, MonadRandom m) => m (ShowdownHistory a)
    randomSample = do
      as <- getRandoms
      let (l:r:[]) = take 2 as
      return [(l,r)]
    markovGuess :: (Ord a, Showdown a) => ShowdownHistory a -> a
    markovGuess = beats . snd . head

--runStateT (evalRandT (play (markovChainStrategy::RpsStrategy)) (mkStdGen 123)) []

runShowdownStrategies :: (Showdown a, Ord a, Show a, Monad m) =>
    ShowdownStrategy a m -> ShowdownStrategy a m -> WithShowdownHistory a Outcome m
runShowdownStrategies strategy1 strategy2 = do
  history <- get
  move1 <- play strategy1
  modify' $ map swap
  move2 <- play strategy2
  let game = (move1, move2)
      outcome = showdown move1 move2
  put $ game : history
--  liftIO $ putStrLn $ show game ++ " -> " ++ show outcome
  return outcome

type Stats a = M.Map a Int
type TestResult a = (Stats Outcome, ShowdownHistory a)

testShowdownStrategies :: (Showdown a, Show a, Functor m, Monad m) =>
    Int -> ShowdownStrategy a m -> ShowdownStrategy a m ->
        WithShowdownHistory a (Stats Outcome) m
testShowdownStrategies howMany strategy1 strategy2 =
  toStats <$> runShowdowns
  where
    runShowdowns = replicateM howMany $ runShowdownStrategies strategy1 strategy2
    toStats :: [Outcome] -> Stats Outcome
    toStats = MS.toMap . MS.fromList

runTestShowdownStrategies :: (Showdown a, Ord a, Show a, RandomGen g, Functor m, Monad m) =>
    Int -> g -> ShowdownStrategy a m -> ShowdownStrategy a m -> m (TestResult a)
runTestShowdownStrategies howMany gen strategy1 strategy2 =
  runStateT (evalRandT (testShowdownStrategies howMany strategy1 strategy2) gen) []

-- runTestShowdownStrategies 10 (mkStdGen 123) (randomStrategy::RpsStrategy) mimicStrategy
-- runTestShowdownStrategies 10 (mkStdGen 123) (mimicStrategy::RpsStrategy) mimicStrategy
-- runTestShowdownStrategies 10 (mkStdGen 123) (markovChainStrategy::RpsStrategy) mimicStrategy

main :: IO ()
main = do
  putStr "How many showdowns?"
  input <- getLine
  let howMany = read input :: Int
  gen <- newStdGen
  results <- runTestShowdownStrategies howMany gen (markovChainStrategy::RpsStrategy) userInputStrategy
  print results
