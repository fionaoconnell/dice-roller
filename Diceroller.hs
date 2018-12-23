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


--(*d*) :: Int -> Int -> RandState Int
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- The parser.

type Quantity = Int
type Sides = Int
type Number = Int

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

-- Running the parser-----------------------------------------------------------
-- Parse function from Lab6, rewrite it later.
parse :: String -> Maybe RollExp
parse str =
    case (completeParses, incompleteParses) of
        ([(result, "")], _  ) -> Just result  -- Only complete result.
        ([]            , [] ) -> Nothing--Left $ "No parse."
        ([]            , _:_) -> Nothing--Left $ "Incomplete parse. Unparsed: " ++ show leastRemaining
        (_:_           , _  ) -> Nothing--Left $ "Ambiguous parse: " ++ show completeParses
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

--  runInputT defaultSettings superRepl


evalRollExp :: Maybe RollExp -> Maybe (IO Int)
evalRollExp exp =
    case exp of
        Just (Roll_d_ n die) -> Just $ newStdGen >>= (\gen -> return (runRandom (rollNTimes n die gen) gen))
        Just (Modifier n) -> Just $ return n
        Just (Plus  e1 e2) -> liftA2 (liftA2 (+)) (evalRollExp (Just e1)) (evalRollExp (Just e2))
        Just (Minus e1 e2) -> liftA2 (liftA2 subtract) (evalRollExp (Just e1)) (evalRollExp (Just e2))
        Nothing -> Nothing
--
-- -- Use if Haskeline doesn't work.
-- simpleRepl :: IO ()
-- simpleRepl = do
--     putStr "> "
--     hFlush stdout
--     line <- getLine
--     case handleReplLine line of
--         Nothing        -> return ()    -- Time to quit.
--         Just resultStr -> do
--             resultStr
--             simpleRepl
--
-- handleReplLine :: String -> Maybe (IO String)
-- handleReplLine line =
--     if line `elem` ["q", ":q", "quit", ":quit", "exit"] then
--         Nothing
--     else
--         let
--             unparsedStr =
--                 case parse line of
--                     Right parsed -> unparseRollExp parsed ++ "\n"
--                     _            -> ""
--         in
--         Just $
--             liftA2 (++) (pure  ((show . parse $ line) ++ "\n" ++ unparsedStr)) (showParseEval line)
--
--
-- unparseRollExp :: RollExp -> String
-- unparseRollExp (Roll_d_ n die)     = show n ++ "d" ++ show die
-- unparseRollExp (Modifier n) = show n
-- -- unparseRollExp (Neg    e1)    = "-" ++ unparseRollExp e1
-- unparseRollExp (Plus   e1 e2) = "(" ++ unparseRollExp e1 ++ " + " ++ unparseRollExp e2 ++ ")"
-- unparseRollExp (Minus  e1 e2) = "(" ++ unparseRollExp e1 ++ " - " ++ unparseRollExp e2 ++ ")"
--
-- -- If second argument is Left, prefix the error message
-- -- with the first argument. Otherwise, pass-through.
-- prefixError :: String -> Either String a -> Either String a
-- prefixError prefix (Left errMsg) = Left (prefix ++ errMsg)
-- prefixError _      rightResult   = rightResult
--
--
-- tryParseEval :: String -> Either String (IO Int)
-- tryParseEval expStr =
--     parse expStr >>= (prefixError "Evaluation error: " . eval)
--
--
-- -- Convert result to string or return error message.
-- showParseEval :: String -> IO String
-- showParseEval expStr =
--     case tryParseEval expStr of
--       Right result  -> show <$> result
--       Left errorMsg -> pure errorMsg
