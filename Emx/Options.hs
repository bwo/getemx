{-# LANGUAGE NoMonomorphismRestriction #-}
module Emx.Options where
import System.IO 
import System.FilePath
import System.Directory
import Control.Applicative
import Control.Monad.Error
import Emx.Track

data Subs = Txt String | IntSub (Track -> Int) | StrSub (Track -> String)

data Options = Opt {repu, repapo, get_art :: Bool, dldir :: String,
                    dlfmt, dlfmt_m :: [Subs]}

lookupsub "a" = return $ StrSub artist
lookupsub "A" = return $ StrSub album
lookupsub "D" = return $ IntSub disccount
lookupsub "d" = return $ IntSub discnum
lookupsub "e" = return $ StrSub ext
lookupsub "t" = return $ StrSub title
lookupsub "l" = return $ StrSub label
lookupsub "g" = return $ StrSub genre
lookupsub "n" = return $ StrSub tracknum
lookupsub x = throwError $ "Unrecognized format option: \""++x++"\""

subfromstring s = go s []
    where
      go "" a = return $ reverse a
      go "%" a = throwError $ "Unescaped parenthesis at end of option string \""++s++"\""
      go ('%':'(':cs) a = do
        (kind, rest) <- return $ break (==')') cs
        when (null rest) $ throwError $ "Unclosed parenthesis in option string \""++s++"\""
        sub <- lookupsub kind
        go rest (sub:a)
      go ('%':'%':cs) as = 
          case as of
            Txt s:r -> go cs ((Txt $ s++"%"):r)
            _ -> go cs (Txt "%":as)
      go s as =
          case as of
            Txt s:r -> go rest ((Txt $ s++run):as)
            _ -> go rest (Txt run:as)
          where
            (run,rest) = break (=='%') s

(Right default_dlf) = subfromstring "%(a)/%(A)/%(a) - %(A) - %(n) - %(t)"
(Right default_dlfm) = subfromstring "%(a)/%(A): %(D)/%(a) - %(A): %(D) - %(n) - %(t)"

strip rem = reverse.snd.span (`elem` rem).reverse.snd.span (`elem` rem)
split s splite = (before, after)
    where
      (before,rest) = break (==splite) s
      (_,after) = span (==splite) rest
stripw = strip " \t\n"

optline o line = ps (stripw s) (stripw e)
    where
      (s,e) = split line '='
      ps ('#':rest) _ = return o
      ps s e = 
          case s of 
            "replace_underscores" -> bconv (\x -> o {repu = x}) s e
            "replace_apostrophe_identity" -> bconv (\x -> o {repapo = x}) s e
            "get_art" -> bconv (\x -> o {get_art = x}) s e
            "dlfmt" -> sconv (\x -> o {dlfmt = x}) s e
            "dlfmt_multidisc" -> sconv (\x -> o {dlfmt_m = x}) s e
            "dldir" -> return $ o {dldir = e}
            "" -> return o
            x -> throwError $ "Unrecognized option: " ++ x
      bconv u oname v 
          | v `elem` ["f", "false"] = return $ u False
          | v `elem` ["t", "true"]  = return $ u True
          | otherwise = throwError $ "Boolean values must be one of 'f', 't', 'true', and 'false', not `"++v++"\', in option "++oname
      sconv u on v = catchError (u `liftM` (subfromstring v))
                     (\t -> throwError $ t++" (error in option \""++on++"\")")

readopts = do
  dotfile <- fmap (</> ".emxdownloader") getHomeDirectory
  h <- openFile dotfile ReadMode
  contents <- fmap lines $ hGetContents h
  curdir <- getCurrentDirectory
  let default_options = Opt True True True curdir default_dlf default_dlfm
  return (foldM optline default_options contents)
