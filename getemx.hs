import System (getArgs, exitFailure)
import System.IO 
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (readTime)
import Control.Monad.Error
import System.FilePath
import System.Directory
import System.Locale (defaultTimeLocale)
import Prelude hiding (catch)


import Emx.Dl
import Emx.Options
import Emx.Track
import Emx.Emx

-- all tracks for the same album have the same artwork.
artdl :: (IO (String, String) -> IO ()) -> IO (String, String) -> IO ()
artdl dler remloc = do
  (rem, loc) <- remloc
  exists <- doesFileExist loc
  unless exists $ dler remloc
  return ()

findPathAndName p = findExecutable p >>= maybe (return Nothing) (\path -> return $ Just (path,p)) 

preptrack opts track = do
  made <- foldM joinandtest (dldir opts) $ splitPath ldir
  return (dlurl track, combine made lname)
    where
      joinandtest :: String -> FilePath -> IO String
      joinandtest sofar component = do
        let jed = combine sofar component
        exists <- doesDirectoryExist jed
        unless exists (createDirectory jed)
        return jed
      (ldir, lname) = splitFileName $ if disccount track == 1 
                                      then getlocalname (dlfmt opts) track []
                                      else getlocalname (dlfmt_m opts) track []
      getlocalname [] tr [] = ext tr
      getlocalname [] tr (a:acc) 
          | a == ext tr = concat $ reverse (a:acc)
          | otherwise = concat $ reverse (ext tr:a:acc)
      getlocalname (Txt s:st) tr acc = getlocalname st tr (s:acc)
      getlocalname (IntSub s:st) tr acc = getlocalname st tr ((show$s tr):acc)
      getlocalname (StrSub s:st) tr acc = getlocalname st tr (s tr:acc)

prepart opts track = do
  trackdir <- (liftM $dropFileName.snd) $ preptrack opts track
  let artname = takeFileName $ arturl track
  return (arturl track, combine trackdir artname)

process' opts f = 
    do 
      (action, expiry, tracks) <- readfile f (repu opts) (repapo opts)
      unless (action == "download") $ throwError ("Unrecognized EMX action: "++action++". Skipping file "++f)
      now <- liftIO getCurrentTime
      let exp = readTime defaultTimeLocale "%m/%d/%Y %H:%M" expiry
      unless (now < exp) $ throwError ("EMX file "++f++" has expired!")
      dlp <- liftIO $ findPathAndName "wget" `mplus` findPathAndName "curl"
      let dler = case dlp of
                   Nothing -> dlnative
                   Just (path, "wget") -> dlwget path
                   Just (path, "curl") -> dlcurl path
      mapM_ (liftIO.dler.preptrack opts) tracks
      when (get_art opts) $ mapM_ (liftIO.artdl dler.prepart opts) tracks
      return ()

process o f = do               
  r <- runErrorT $ process' o f
  case r of
    (Left e) -> putStrLn e
    _ -> return ()

main :: IO ()
main = do
  args <- getArgs  
  eopts <- readopts
  case eopts of
    (Right opts) -> mapM_ (process opts) args
    (Left e) -> do 
               putStrLn $  "Error reading options: " ++ e
               exitFailure
