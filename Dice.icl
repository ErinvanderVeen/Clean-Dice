module Dice

import StdEnv

import Control.Monad
import Data.Functor
import Data.Func
import Data.List
import Math.Random
import System.IO
import System.Time
from Text import class Text(trim, split), instance Text String

:: Dice = Dice Int Int Int

// Reads a line from input and splits it on space
// i.e. 1d20+4 1d5 becomes: [1d20+4, 1d5]
readInput :: IO [String]
readInput = split " " <$> trim <$> getLine

parseDice :: [Char] -> Dice
parseDice s
#! (n, r) = (takeWhile ((<>) 'd') s, tl $ dropWhile ((<>) 'd') s)
#! (v, r) = (takeWhile (not o sign) r, dropWhile (not o sign) s)
#! (s, a) = case r of
	[] -> ('+', ['0'])
	[s:a] -> (s, a)
#! a = toInt $ toString a
#! a = if (s == '-') (-1 * a) a
= Dice (toInt $ toString n) (toInt $ toString v) a
where
	sign :: Char -> Bool
	sign '-' = True
	sign '+' = True
	sign _ = False

multDice :: Dice -> [Int]
multDice (Dice n v a) = repeatn n v

rollDice :: [Dice] -> IO [Int]
rollDice ds = getRandInts >>= \is -> fst <$> mapStM rollDice` ds is
where
	getSeed :: IO Int
	getSeed = toInt <$> withWorld time

	getRandInts :: IO [Int]
	getRandInts = getSeed >>= return o genRandInt

	rollDice` :: Dice [Int] -> IO (Int, [Int])
	rollDice` d=:(Dice n v a) is =
		    sum <$> (sequence $ zipWith rollDie (multDice d) is)
		>>= \i -> putStr (toString n)
		>>| putStr "d"
		>>| putStr (toString v)
		>>| putStr ((if (a >= 0) "+" "-") +++ toString a +++ ": ")
		>>| putStrLn (toString (i + a))
		>>| return (i + a, drop n is)

	rollDie :: Int Int -> IO Int
	rollDie i r =
		    putStr "d"
		>>| putStr (toString i)
		>>| putStr ": "
		>>| putStrLn (toString (r rem i))
		>>| return (r rem i)

printTotal :: [Int] -> IO ()
printTotal xs = putStrLn (toString (sum xs))

Start :: *World -> *World
Start world = execIO (forever ((map parseDice <$> map fromString <$> readInput) >>= rollDice >>= printTotal)) world
