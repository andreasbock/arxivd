module Format.Format where

--import qualified Brick.Main as Brick
import Control.Monad (when, zipWithM)
import Data.List.Split
import qualified Data.List as List
import Text.Printf

type Title = String
type Summary = String
type ArxivId = String

data Entry = Entry Title Summary ArxivId deriving Show

-- ANSI color escape code
colorCode :: String
colorCode = "\x1b["

-- A format string for printf for printing text in color
-- Format is: color code| color number | bold code | color code terminator | string
--            ...       | %d           | %s        | m                     | %s
simpleColorFormatString :: String
simpleColorFormatString = colorCode ++ "%d%sm%s" ++ colorCode ++ "0m"

-- List of supported color names
colorNames :: [String]
colorNames = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"]

-- |Format a string with a (possibly bolded) color
formatWithColor :: String -> String -> Bool -> String
formatWithColor string color bold =
    case List.elemIndex color colorNames of
        Just i  -> printf simpleColorFormatString (i + 30) boldCode string
                   where boldCode = if bold then ";1"
                                            else ""
        Nothing -> string

entryMarker :: String
entryMarker = formatWithColor "▶ " "green" True

formatTest :: Entry -> IO ()
formatTest (Entry title summary id) = do
    let idString    = formatWithColor id    "yellow" True
    putStrLn (entryMarker ++ "\"" ++ title ++ "\" (arxiv id: " ++ idString ++ ")")
    mapM_ (putStrLn . (\l -> "    " ++ l)) (splitOn "\n" summary)

printTest :: IO ()
printTest = do
    mapM_ (putStrLn . (\n -> formatWithColor "LolLeR" n False)) colorNames
    mapM_ (putStrLn . (\n -> formatWithColor "LolLeR" n True)) colorNames

testEntries :: [Entry]
testEntries = [Entry "Title1" "This oirgoi\nbrgjlrg\nlsnrg" "075329",
               Entry "Title2" "oubrsg\nbkrg\nubdglinf\noir" "93203"]

-- |The main loop for browsing arXiv digests
main :: IO ()
main = do renderEntry (Entry "Title1" "This oirgoi\nbrgjlrg\nlsnrg" "075329") True
          renderEntry (Entry "Title1" "This oirgoi\nbrgjlrg\nlsnrg" "075329") False
{-main = sequence (renderEntries 0 testEntries)-}
{-main = runCurses $ do-}
{-    setEcho False-}
{-    window <- defaultWindow-}
{-    userLoop window-}

{-userLoop :: Window -> IO ()-}
{-userLoop window = do-}
{-    eventHandler-}
{-    updateWindow window-}
{-    renderEntries testEntries-}
{-    render-}
{-    userLoop-}

{-handleEvents :: Window -> IO ()-}
{-handleEvents window = do-}
{-    event <- getEvent window Nothing-}
{-    case event of-}
{-        Just event' -> print event'-}
{-        Nothing     -> return ()-}

renderEntry :: Entry -> Bool -> IO ()
renderEntry (Entry title summary id) expand = do
    let idString = formatWithColor id "yellow" True

    putStrLn (entryMarker ++ "\"" ++ title ++ "\" (arxiv id: " ++ idString ++ ")")
    when expand $ mapM_ (putStrLn . (\l -> "    " ++ l)) (splitOn "\n" summary)

-- TODO: Why does this not work with sequence?
renderEntries :: Int -> [Entry] -> IO [()]
renderEntries activeIndex =
    zipWithM (\i e -> renderEntry e (i == activeIndex)) [0..]
