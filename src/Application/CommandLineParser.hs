{-# LANGUAGE QuasiQuotes #-}

module Application.CommandLineParser where

import Control.Monad (when)
import Data.Time.Clock (UTCTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Time (getCurrentTime)
import System.Console.Docopt
import System.Environment (getArgs)
import System.Exit
import Utils.Levenshtein (topMatchesN)

programName :: String
programName = "arxivd"

version :: String
version = "0.1.0"

usageText :: Docopt
usageText = [docoptFile|USAGE.docopt|]

data Date = Date {
    day :: Int,
    month :: Int,
    year :: Integer
} deriving Show

data Options = Options {
    subjects   :: [String],
    published  :: Date,
    title      :: String,
    ignoreCase :: Bool
} deriving Show

currentDate :: IO (Integer, Int, Int)
currentDate = fmap (toGregorian . utctDay) getCurrentTime

processArgWithDefault :: Arguments -> Option -> a -> (String -> a) -> a
processArgWithDefault args opt def processor =
    case getArg args opt of
        Just v -> processor v
        _      -> def

processSubjects :: String -> [String]
processSubjects "" = []
processSubjects s  = splitOn "," s

processPublished :: String -> IO Date
processPublished _ = currentDate >>= Date

{-processDate :: String -> Maybe DateTime-}
{-processDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"-}

-- |Process the command line using a given docopt description and a list of strings
processCommandLine :: Docopt -> [String] -> IO Options
processCommandLine usage cmdline = do
    args <- parseArgsOrExit usage cmdline
    let getArgOrExit = getArgOrExitWith usage

    when (args `isPresent` longOption "help") $ exitWithUsage usage
    when (args `isPresent` longOption "version") $ putStrLn (programName ++ " v" ++ version)
        >> exitSuccess

    let processArgWithDefault' = processArgWithDefault args
    let subjects   = processArgWithDefault' (longOption "subjects") [] processSubjects
    let published  = processArgWithDefault' (longOption "published") "" processPublished
    let title      = processArgWithDefault' (longOption "title") "" id
    let ignoreCase = isPresent args (longOption "ignore-case")

    return $ Options subjects title ignoreCase

suggestCorrections :: [String] -> String -> Int -> IO ()
suggestCorrections ss s n = do
    let matches = map fst $ topMatchesN ss s n
    let suffix  = if length matches < 2
                  then "" ++ head matches
                  else intercalate ", " (init matches) ++ " or " ++ last matches

    putStrLn $ "Did you mean: " ++ suffix ++ "?"

main :: IO ()
main = do
    args <- getArgs
    options <- processCommandLine usageText args
    print options
