{-# LANGUAGE QuasiQuotes #-}

module Application.CommandLineParser where

import Control.Monad (when)
import Data.List (intercalate)
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

-- |Process the command line using a given docopt description and a list of strings
processCommandLine :: Docopt -> [String] -> IO ()
processCommandLine usage cmdline = do
    args <- parseArgsOrExit usage cmdline
    let getArgOrExit = getArgOrExitWith usage

    when (args `isPresent` longOption "help") $ exitWithUsage usage
    when (args `isPresent` longOption "version") $ putStrLn (programName ++ " v" ++ version)
        >> exitSuccess
    when (args `isPresent` longOption "subject") $ do
        name <- args `getArgOrExit` longOption "subject"
        putStrLn ("Subject: " ++ name)

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
    processCommandLine usageText args
