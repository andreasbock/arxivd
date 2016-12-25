{-# LANGUAGE QuasiQuotes #-}

module Application.CommandLineParser where

import Control.Monad (when)
import System.Console.Docopt
import System.Environment (getArgs)
import System.Exit

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

main :: IO ()
main = do
    args <- getArgs
    processCommandLine usageText args
