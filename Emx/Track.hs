module Emx.Track where

data Track = Tr {artist, album, title, ext, dlurl, label, arturl, 
                 tracknum :: String,
                 disccount, discnum, trackcount :: Int } deriving Show
