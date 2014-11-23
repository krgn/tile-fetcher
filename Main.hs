{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Data.List (intersperse)
import Control.Monad (forM_, unless)
import Network.HTTP
import Network.Browser
import Network.URI
import System.Directory
import qualified Data.ByteString.Lazy as L

main = do
  urls <- readFile "export.txt" >>= return . lines

  -- create the directory structure
  forM_ urls (mkdirs . splitPath . parseUrl)

  let ps = partition 8 urls
      
  -- download the files with 8 threads
  forM_ (tail ps) (forkIO . get)

  get $ head ps


get :: [String] -> IO ()
get = mapM_ get'
  where
    get' url = do
      let loc = splitPath $ parseUrl url
          pth = (baseDir ++ dest loc)
      exists <- doesFileExist pth

      if exists
        then putStrLn $ pth ++ " exists already. skipping.."
        else fetch url >>= L.writeFile (baseDir ++ dest loc)

    
dest :: [String] -> String
dest = concat . intersperse "/" 


fetch :: String -> IO L.ByteString
fetch url = 
  case parseURI url of
   Just uri -> do
     let req = Request uri GET [] L.empty
     resp <- browse $ do
       request req
     return $ rspBody $ snd resp
   _        -> return L.empty
  
  
parseUrl :: String -> String
parseUrl u = case parseURI u of
  Just uri -> uriPath uri
  _        -> ""


splitPath :: String -> [String]
splitPath p =
  let (w, ns) = span (/= '/') p
  in case ns of
      ('/':ns') -> w : splitPath ns'
      _         -> w : []


mkdirs (_:e:f:_) = do
  eex <- doesDirectoryExist (baseDir ++ e)
  fex <- doesDirectoryExist (baseDir ++ e ++ "/" ++ f)

  unless eex $ createDirectory (baseDir ++ e)
  unless fex $ createDirectory (baseDir ++ e ++ "/" ++ f)
mkdirs _ = return ()


baseDir = "tiles/"

-- | partition a let into p parts
partition p xs =
  let partsize = (length xs) `div` p
  in foldr (partition' partsize) [] [1..p]

  where partition' p' i m = let (f, _)  = splitAt (i * p') xs
                                (_, f') = splitAt ((i - 1) * p') f
                            in  f' : m


