-- A dice roller parser for D&D, based on parsers written in UChicago's CMSC 16100 course.

-- Idea: make it so that it prints what was rolled on each die in addition to totals

module Main where

import State161
import System.Random
import Control.Monad
import Control.Applicative hiding (many)
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP hiding (get)
import           System.Console.Haskeline
import           System.Environment
import           System.IO


------This is the random number setup provided in Lab5, maybe rework later.-----
type RandState a = State StdGen a

runRandom :: RandState a -> StdGen -> a
runRandom (State f) s =
    res
    where (res, state) = f s
------------------------- The random stuff. ------------------------------------
randR :: Random a => (a, a) -> RandState a
randR (low, high) = do
      gen <- get
      let (x, gen') = randomR (low, high) gen
      put gen'
      return x

rolladie :: Int -> RandState Int
rolladie sides = do
  d1 <- randR (1, sides)
  pure d1


rollNTimes :: Int -> Int -> StdGen -> RandState Int
rollNTimes n sides gen = do
   let (ndice, stdgen) = runState (replicateM n (rolladie sides)) gen
   pure $ foldr (+) 0 ndice

--------------------------------------------------------------------------------
-- The parser.

data RollExp
    = Roll_d_ Int Int
    | Modifier Int
    | Plus RollExp RollExp
    | Minus RollExp RollExp
    deriving (Eq, Show)

-- Does the same thing as sepBy1 for exactly 2 occurences of the parser.
sepBy2 :: ReadP a -> ReadP sep -> ReadP [a]
sepBy2 p sep = liftM2 (:) p (count 1 (sep >> p))

parseInt :: ReadP Int
parseInt = skipSpaces *> (read <$> munch1 isDigit) <* skipSpaces

parsePlus :: ReadP (RollExp -> RollExp -> RollExp)
parsePlus = skipSpaces *> char '+' *> pure Plus

parseMinus :: ReadP (RollExp -> RollExp -> RollExp)
parseMinus = skipSpaces *> char '-' *> pure Minus

parseRoll_d_ :: ReadP RollExp
parseRoll_d_ = do
  parsedie <- sepBy2 (parseInt) (char 'd')
  let n = head parsedie
  let sides = last parsedie
  pure $ Roll_d_ n sides


parseRoll :: ReadP RollExp
parseRoll = chainl1 (parseRoll_d_ <++ (Modifier <$> parseInt)) (parsePlus +++ parseMinus)

evalRollExp :: Maybe RollExp -> Maybe (IO Int)
evalRollExp exp =
    case exp of
        Just (Roll_d_ n die) -> Just $ newStdGen >>= (\gen -> return (runRandom (rollNTimes n die gen) gen))
        Just (Modifier n) -> Just $ return n
        Just (Plus  e1 e2) -> liftA2 (liftA2 (+)) (evalRollExp (Just e1)) (evalRollExp (Just e2))
        Just (Minus e1 e2) -> liftA2 (liftA2 subtract) (evalRollExp (Just e1)) (evalRollExp (Just e2))
        Nothing -> Nothing

-- Parse function from Lab6, rewrite it later.
parse :: String -> Maybe RollExp
parse str =
    case (completeParses, incompleteParses) of
        ([(result, "")], _  ) -> Just result  -- Only complete result.
        ([]            , [] ) -> Nothing
        ([]            , _:_) -> Nothing
        (_:_           , _  ) -> Nothing
    where
        parses = readP_to_S parseRoll str
        (completeParses, incompleteParses) =
            partition (\(_, remaining) -> remaining == "") parses
        leastRemaining = minimumBy (comparing length) . map snd $ incompleteParses

main :: IO ()
main = do
     putStr "> "
     hFlush stdout
     line <- getLine
     let Just out = fmap (fmap show) $ evalRollExp $ parse line
     realout <- out
     putStrLn $ realout
     main
