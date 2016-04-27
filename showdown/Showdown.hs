module Showdown where

import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.MultiSet as MS
import System.Random (newStdGen)
import Data.Random.Extras
import Data.Random.Source.DevRandom
import Data.RVar
import Data.MarkovChain
import qualified System.Random as Random
import qualified Data.List as L
import Data.Maybe
import Data.Tuple (swap)

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

type WithShowdownHistory a b = StateT (ShowdownHistory a) IO b

newtype ShowdownStrategy a =
  ShowdownStrategy { play :: WithShowdownHistory a a }

randomBoundedEnum :: (Bounded a, Enum a) => IO a
randomBoundedEnum = runRVar choose DevRandom
  where choose = choice [minBound .. maxBound]

randomRpsStrategy :: ShowdownStrategy RPS
randomRpsStrategy = ShowdownStrategy $ liftIO randomBoundedEnum

--runStateT (replicateM 10 (play randomRpsStrategy)) []

showdownStrategy :: (Bounded a, Enum a) =>
    (ShowdownHistory a -> a) -> ShowdownStrategy a
showdownStrategy f = ShowdownStrategy $ do
  history <- get
  if null history then liftIO randomBoundedEnum
  else return $ f history

cyclicRpsStrategy :: ShowdownStrategy RPS
cyclicRpsStrategy = showdownStrategy $ next . fst . last
--ShowdownStrategy $ do
--  history <- get
--  if null history then liftIO randomBoundedEnum
--  else return . next . fst $ last history
  where
    next x = if x == maxBound then minBound else succ x

--runStateT (play cyclicRpsStrategy) [(Scissors,Paper)]

mimicRpsStrategy :: ShowdownStrategy RPS
mimicRpsStrategy = showdownStrategy $ snd . last
--ShowdownStrategy $ do
--  history <- get
--  if null history then liftIO randomBoundedEnum
--  else return . snd $ last history

--runStateT (play mimicRpsStrategy) [(Scissors,Paper)]

userInputRpsStrategy :: ShowdownStrategy RPS
userInputRpsStrategy = ShowdownStrategy $ do
  liftIO $ putStr "Choose (r)ock, (p)aper or (s)cissors: "
  input <- liftIO getLine
  return $ readChar (head input)

--runStateT (play userInputRpsStrategy) []

markovChainRpsStrategy :: ShowdownStrategy RPS
markovChainRpsStrategy = ShowdownStrategy $ do
  history <- get
  liftIO $ do
    gen <- newStdGen
    trainingSeq <- if null history then randomSample else return $ lastN 10 history
    return . beats . snd . head $ run 3 trainingSeq 0 gen
  where
    randomSample = do
      l <- randomBoundedEnum
      r <- randomBoundedEnum
      return [(l,r)]
    lastN n xs = drop (length xs - n) xs

--runStateT (play markovChainRpsStrategy) []

runShowdownStrategies :: (Showdown a, Show a) =>
    ShowdownStrategy a -> ShowdownStrategy a -> WithShowdownHistory a Outcome
runShowdownStrategies strategy1 strategy2 = do
  move1 <- play strategy1
  history <- get
  modify' $ map swap
  move2 <- play strategy2
  put history
  liftIO $ print (move1, move2)
  modify' $ flip (++) [(move1, move2)]
  h <- get
  return $ showdown move1 move2

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

runTestShowdownStrategies :: (Showdown a, Show a) =>
    Int -> ShowdownStrategy a -> ShowdownStrategy a -> IO (TestResult a)
runTestShowdownStrategies howMany strategy1 strategy2 =
  runStateT (testShowdownStrategies howMany strategy1 strategy2) []

-- runTestShowdownStrategies 10 randomRpsStrategy mimicRpsStrategy
-- runTestShowdownStrategies 10 mimicRpsStrategy mimicRpsStrategy
-- runTestShowdownStrategies 10 markovChainRpsStrategy mimicRpsStrategy

--foo = take 100 $ run 2 "The sad cat sat on the mat. " 0 (Random.mkStdGen 123)
--bar = take 100 $ run 2 [1,2,3,1,2,3,1,2,3] 0 (Random.mkStdGen 123)
--baz = take 10 $ run 2 [1,2,2,1,3,1,3] 0 (Random.mkStdGen 123)