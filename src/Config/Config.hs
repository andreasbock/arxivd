module Config where

import Control.Monad (join)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (liftIO)
import Data.ConfigFile (emptyCP, readfile, get, CPError)
import Data.List.Split (splitOn)

-- Configuration record
data Config = Config {
    subjects :: [String]
} deriving Show

emptyConfig :: Config
emptyConfig = Config []

-- |Read the configuration specified by the given filename
readConfig :: String -> IO Config
readConfig filename = do
    rv <- runExceptT $
        do cp <- join $ liftIO $ readfile emptyCP filename
           get cp "DEFAULT" "subjects"
    parseConfig rv

parseConfig :: Either CPError String -> IO Config
parseConfig (Left (info, msg)) = return emptyConfig
parseConfig (Right s)          = return . Config $ parseSubjects s

parseSubjects :: String -> [String]
parseSubjects = splitOn ","

main :: IO ()
main = do
    config <- readConfig "test.cfg"
    print config
