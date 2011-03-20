module Emx.Dl where

import System.FilePath
import System.Directory
import System.IO
import Control.Applicative
import System.Process (runProcess, waitForProcess)
import Text.Printf (printf)
import Control.Monad
import Network.Curl
import Network.Curl.Easy
import Network.Curl.Opts
import Data.Ratio
import Data.Char
import Data.List
import Data.IORef


dl :: String -> String -> IO (String, String) -> IO ()
dl a epath remloc = do
  (rem, loc) <- remloc
  notexists <- not <$> doesFileExist loc
  when notexists $ runProcess epath [rem, a, loc] Nothing Nothing Nothing Nothing Nothing >>= waitForProcess >> return ()
dlcurl = dl "-o"
dlwget = dl "-O"

-- native download. How likely is it that curl(1) won't be available
-- but libcurl will be?
colheaders :: IORef Integer -> String -> IO ()
colheaders tot s = do
  (key, val) <- return $ parseHeader $ map toLower s
  when (key == "content-length") $ writeIORef tot $ read val

writebody sofar tot handle s = do
  clen <- readIORef tot
  modifyIORef sofar (+ genericLength s)
  sof <- readIORef sofar
  if clen /= 0
     then do
       if sof == clen 
          then putStrLn "\b\b\b100%"
          else putStr $ printf "\b\b\b%2.0f%%"  (fromRational (100*sof%clen)::Float)
       hFlush stdout
     else when (sof == clen) $ putStrLn "... Done"
  hPutStr handle s
  return ()

dlnative :: IO (String, String) -> IO ()
dlnative remloc = do 
      (rem, loc) <- remloc
      tot <- newIORef (0::Integer)
      sofar <- newIORef (0::Integer)
      curl <- initialize
      h <- openFile loc WriteMode
      setopts curl [CurlHeaderFunction $ callbackWriter (colheaders tot),
                    CurlWriteFunction $ callbackWriter (writebody sofar tot h),
                    CurlURL rem,
                    CurlFailOnError True]
      setDefaultSSLOpts curl rem
      putStrLn $ printf "Downloading %s ..." rem
      putStrLn $ printf "To %s" loc
      perform curl
      rspCode <- getResponseCode curl
      hFlush h
      hClose h
      return ()
