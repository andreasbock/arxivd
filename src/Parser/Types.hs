module Parser.Types where

import Data.DateTime

data Author = Author {
    name :: String, -- Or first and last name
    affiliation :: String
}

data Entry = Entry {
    id         :: String,
    updated    :: DateTime,
    published  :: DateTime,
    title      :: String,
    summary    :: String,
    authors    :: [Author],
    doi        :: String,
    comment    :: String,
    journalRef :: String
}
