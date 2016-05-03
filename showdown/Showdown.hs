--{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Showdown where

import Control.Applicative ((<$>))
--import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Random
import qualified Data.Map as M
import qualified Data.MultiSet as MS
import System.Random (RandomGen, newStdGen, randomR)
--import Data.Random.Extras
--import Data.Random.Source.DevRandom
--import Data.RVar
import Data.MarkovChain
import qualified System.Random as Random
import qualified Data.List as L
import Data.Maybe
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

--type WithShowdownHistory a b g = ReaderT (RandomGen g) (StateT (ShowdownHistory a) IO) b
--type WithShowdownHistory a = ReaderT StdGen (StateT (ShowdownHistory a) IO)
--type WithShowdownHistory a = RandT StdGen (StateT (ShowdownHistory a) Identity)
--type WithShowdownHistory a = StateT (ShowdownHistory a) (RandT StdGen Identity)
type WithShowdownHistory a b = --RandT StdGen (StateT (ShowdownHistory a) Identity)
--  forall g. RandT g (StateT (ShowdownHistory a) Identity) b
  (Ord a, RandomGen g) => RandT g (StateT (ShowdownHistory a) Identity) b

newtype ShowdownStrategy a =
-- TODO make play :: ShowdownHistory a a -> Rand a ?!
  ShowdownStrategy { play :: WithShowdownHistory a a }

instance Random RPS where
  randomR (a,b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)

--randomBoundedEnum :: (Bounded a, Enum a) => IO a
--randomBoundedEnum = runRVar choose DevRandom
--  where choose = choice [minBound .. maxBound]
--randomBoundedEnum :: (Bounded a, Enum a, Random a, RandomGen g) => Rand g a
randomBoundedEnum :: (Bounded a, Enum a, Random a, MonadRandom m) => m a
randomBoundedEnum = getRandomR (minBound, maxBound)

--type RandRPS = Rand StdGen RPS

randomRpsStrategy :: ShowdownStrategy RPS
randomRpsStrategy = ShowdownStrategy randomBoundedEnum

--runStateT (replicateM 10 (play randomRpsStrategy)) []

showdownStrategy :: (Ord a, Bounded a, Enum a, Random a) =>
    (ShowdownHistory a -> a) -> ShowdownStrategy a
showdownStrategy f = ShowdownStrategy $ do
  history <- get
  if null history then randomBoundedEnum
  else return $ f history

cyclicRpsStrategy :: ShowdownStrategy RPS
cyclicRpsStrategy = showdownStrategy $ next . fst . head
  where
    next x = if x == maxBound then minBound else succ x

--runStateT (play cyclicRpsStrategy) [(Scissors,Paper)]

mimicRpsStrategy :: ShowdownStrategy RPS
mimicRpsStrategy = showdownStrategy $ snd . head

--runStateT (play mimicRpsStrategy) [(Scissors,Paper)]

-- put this into some main function
userInputRpsStrategy :: IO (ShowdownStrategy RPS)
userInputRpsStrategy = do
  putStr "Choose (r)ock, (p)aper or (s)cissors: "
  input <- getLine
  return $ ShowdownStrategy $ return (readChar (head input))

--runStateT (play userInputRpsStrategy) []

newtype MarkovRandomWalk a =
  MarkovRandomWalk { randomWalk :: [a] }

newtype RunMarkovRandomWalk a =
  RunMarkovRandomWalk { runWalk :: (Ord a) => Int -> [a] -> Int-> MarkovRandomWalk a }

instance Random (RunMarkovRandomWalk a) where
  randomR (a,b) g =
    undefined
  random gen = (RunMarkovRandomWalk rw, gen')
    where
      rw size trainingSeq start = MarkovRandomWalk $ run size trainingSeq start gen
      (_, gen') = split gen

markovChainRpsStrategy :: ShowdownStrategy RPS
markovChainRpsStrategy = ShowdownStrategy $ do
  history <- get
--  liftIO $ do
--    gen <- newStdGen
--  (_, gen) <- getRandom
  let gen = mkStdGen 123
  trainingSeq <-
        if null history then randomSample else return $ take 10 history
--  return . beats . snd . head $ runMarkovChain trainingSeq
  markovRandomWalk <- runMarkovRandomWalk
  let markovChain = (runWalk markovRandomWalk) 3 trainingSeq 0
--  let markovChain = runMarkovChain gen trainingSeq
  let guess = markovGuess $ randomWalk markovChain
  return guess
  where
--    randomSample :: (Random a, RandomGen g) => Rand g (ShowdownHistory a)
    randomSample :: (Random a, MonadRandom m) => m (ShowdownHistory a)
    randomSample = do
      as <- getRandoms
      let (l:r:[]) = take 2 as
      return [(l,r)]
    markovGuess :: (Ord a, Showdown a) => ShowdownHistory a -> a
    markovGuess = beats . snd . head
    runMarkovRandomWalk :: (Ord a, MonadRandom m) => m (RunMarkovRandomWalk (a, a))
    runMarkovRandomWalk = getRandom

--runStateT (play markovChainRpsStrategy) []

runShowdownStrategies :: (Showdown a, Ord a, Show a) =>
    ShowdownStrategy a -> ShowdownStrategy a -> WithShowdownHistory a Outcome
runShowdownStrategies strategy1 strategy2 = do
  move1 <- play strategy1
  history <- get
  modify' $ map swap
  move2 <- play strategy2
  let game = (move1, move2)
      outcome = showdown move1 move2
  put $ game : history
--  liftIO $ putStrLn $ show game ++ " -> " ++ show outcome
  return outcome

-- runStateT (runShowdownStrategies randomRpsStrategy randomRpsStrategy) []

twoRandomShowdowns :: WithShowdownHistory RPS (Outcome, Outcome)
twoRandomShowdowns = do
  one <- runShowdownStrategies randomRpsStrategy randomRpsStrategy
  two <- runShowdownStrategies randomRpsStrategy randomRpsStrategy
  return (one, two)

-- runStateT twoRandomShowdowns []

type Stats a = M.Map a Int
type TestResult a = (Stats Outcome, ShowdownHistory a)

testShowdownStrategies :: (Showdown a, Show a) =>
    Int -> ShowdownStrategy a -> ShowdownStrategy a -> WithShowdownHistory a (Stats Outcome)
testShowdownStrategies howMany strategy1 strategy2 =
  toStats <$> runShowdowns
  where
    runShowdowns = replicateM howMany $ runShowdownStrategies strategy1 strategy2
    toStats :: [Outcome] -> Stats Outcome
    toStats = MS.toMap . MS.fromList

runTestShowdownStrategies :: (Showdown a, Ord a, Show a, RandomGen g) =>
    Int -> g -> ShowdownStrategy a -> ShowdownStrategy a -> TestResult a
runTestShowdownStrategies howMany gen strategy1 strategy2 =
--  gen <- getStdGen
--  let gen = mkStdGen 123
  runIdentity (runStateT (evalRandT (testShowdownStrategies howMany strategy1 strategy2) gen) [])

-- runTestShowdownStrategies 10 (mkStdGen 123) randomRpsStrategy mimicRpsStrategy
-- runTestShowdownStrategies 10 (mkStdGen 123) mimicRpsStrategy mimicRpsStrategy
-- runTestShowdownStrategies 10 (mkStdGen 123) markovChainRpsStrategy mimicRpsStrategy

main :: IO ()
main = do
  putStr "How many showdowns?"
  input <- getLine
  gen <- newStdGen
  let howMany = read input :: Int
  uis <- userInputRpsStrategy
  let results = runTestShowdownStrategies howMany gen markovChainRpsStrategy uis
  print results

--foo = take 100 $ run 2 "The sad cat sat on the mat. " 0 (Random.mkStdGen 123)
--bar = take 100 $ run 2 [1,2,3,1,2,3,1,2,3] 0 (Random.mkStdGen 123)
--baz = take 10 $ run 2 [1,2,2,1,3,1,3] 0 (Random.mkStdGen 123)