{-# OPTIONS_GHC -XArrows -XScopedTypeVariables -XNamedFieldPuns #-}

module Emx.Emx where

import System.FilePath
import Data.List (isPrefixOf)
import Control.Monad.Error
import Text.XML.HXT.Core
import Emx.Track

atTag :: (ArrowXml a) => String -> a XmlTree XmlTree
atTag  = deep . hasName 
text :: (ArrowXml a) => a XmlTree String
text = getChildren >>> getText
opttagtext :: (ArrowXml a) => String -> a XmlTree String
opttagtext = (<<<) text . atTag

tagtext t = (opttagtext t >>> arr Right) `orElse` (constA $ Left $  "Bad XML: couldn't find tag "++t)

replace::(Eq a) => [a] -> [a] -> [a] -> [a]
replace [] newSub list = joins newSub list
replace oldSub newSub list = _replace list where
    _replace list@(h:ts) = if isPrefixOf oldSub list
                           then newSub ++ _replace (drop len list)
                           else h : _replace ts
    _replace [] = []
    len = length oldSub

joins::[a] -> [a] -> [a]
joins glue [h] = [h]
joins glue (h:ts) = h : glue ++ joins glue ts
joins _ [] = []

clean repu repapo = pthc . uc . apoc
    where
      pthc = replace [pathSeparator] "_"
      uc = if repu then replace "_" " " else id
      apoc = if repapo then replace "&#039;" "'" else id

mktrack a al e tc t d l au dc dn tn = do
  artist <- a
  album <- al
  ext <- e
  trackcount <- tc
  title <- t
  dlurl <- d
  label <- l
  arturl <- au
  disccount <- dc
  discnum <- dn
  tracknum <- tn
  return Tr {artist, album, ext, trackcount, title, dlurl, label, arturl,
             disccount, discnum, tracknum}

gettrack repu repapo = atTag "TRACK" >>>
           (proc t -> do
              artist <- c <<< tagtext "ARTIST" -< t
              album <- c <<< tagtext "ALBUM"-< t
              title <- c <<< tagtext "TITLE"-< t
              ext <- tagtext "EXTENSION" -< t
              dlurl <- tagtext "TRACKURL" -< t
              label <- c <<< tagtext "LABEL" -< t
              arturl <- tagtext "ALBUMART" -< t
              disccount <- rtag "DISCCOUNT" -< t
              discnum <- rtag "DISCNUM" -< t
              tc <- rtag "TRACKCOUNT" -< t
              tracknum <- tr <<< rtag "TRACKNUM" -< t
              returnA -< mktrack artist album ext tc title dlurl label arturl disccount discnum tracknum)
    where
      c = right $ arr $ clean repu repapo
      r t = arr $ \i -> do
              v <- i
              case (reads v) of
                [] -> throwError $ "Bad XML: couldn't parse int in tag "++t
                [(x::Int,_)] -> return x
      rtag t = (tagtext t) >>> (r t)
      tr = right $ arr $ (take 2 . show)

collect repu repapo = atTag "PACKAGE" >>> (tagtext "ACTION" &&& tagtext "EXP_DATE" &&& (listA $ gettrack repu repapo)) >>> arr f
    where
      f (action, (exp, tracks)) = do
        t <- sequence tracks
        a <- action
        e <- exp
        return (a,e,t)

parseXML = readDocument [withValidate False]

readfile f repu repapo = do
  [r] <- liftIO $ runX (parseXML f >>> (collect repu repapo))
  case r of 
    Right s -> return s
    Left e -> throwError e
