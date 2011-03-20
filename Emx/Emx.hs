{-# LANGUAGE Arrows, ScopedTypeVariables, NamedFieldPuns #-}

module Emx.Emx where
import Control.Applicative
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

tagtext t = (opttagtext t >>> arr Right) `orElse` constA (Left $  "Bad XML: couldn't find tag "++t)

replace::(Eq a) => [a] -> [a] -> [a] -> [a]
replace [] newSub list = joins newSub list
replace oldSub newSub list = _replace list where
    _replace list@(h:ts) = if oldSub `isPrefixOf` list
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

instance Applicative (Either a) where
    pure = Right
    (Right f) <*> (Right x) = Right $ f x
    (Left f) <*> _ = Left f
    _ <*> (Left x) = Left x

gettrack repu repapo = atTag "TRACK" >>>
           (proc t -> do
              ar <- c <<< tagtext "ARTIST" -< t
              al <- c <<< tagtext "ALBUM"-< t
              ti <- c <<< tagtext "TITLE"-< t
              ext <- tagtext "EXTENSION" -< t
              url <- tagtext "TRACKURL" -< t
              lbl <- c <<< tagtext "LABEL" -< t
              art <- tagtext "ALBUMART" -< t
              dc <- rtag "DISCCOUNT" -< t
              dn <- rtag "DISCNUM" -< t
              tc <- rtag "TRACKCOUNT" -< t
              tn <- tr <<< rtag "TRACKNUM" -< t
              returnA -< Tr <$> ar <*> al <*> ti <*> ext <*> url <*> lbl <*> art <*> tn <*> dc <*> dn <*> tc)
    where
      c = right $ arr $ clean repu repapo
      r t = arr $ \i -> do
              v <- i
              case reads v of
                [] -> throwError $ "Bad XML: couldn't parse int in tag "++t
                [(x::Int,_)] -> return x
      rtag t = tagtext t >>> r t
      tr = right $ arr (take 2 . show) -- bad assumption: < 100 tracks

collect repu repapo = atTag "PACKAGE" >>> (tagtext "ACTION" &&& tagtext "EXP_DATE" &&& listA (gettrack repu repapo)) >>> arr f
    where
      f (action, (exp, tracks)) = do
        t <- sequence tracks
        a <- action
        e <- exp
        return (a,e,t)

parseXML = readDocument [withValidate False]

readfile f repu repapo = do
  r <- liftIO $ runX (parseXML f >>> collect repu repapo)
  case r of 
    [] -> throwError $ "Couldn't parse emx file "++f++" at all!"
    [Right s] -> return s
    [Left e] -> throwError e
