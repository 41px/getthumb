{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- sample.js:
-- {
--     "elasticsearch":{
-- 	"thumb":"https://hub.docker.com/public/images/official/elasticsearch.png",
-- 	"helpurl": "https://hub.docker.com/_/elasticsearch/",
-- 	"description":"Elasticsearch is a powerful open source search and analytics engine that makes data easy to explore."
--     },
--     "nginx":{
-- 	"thumb":"https://hub.docker.com/public/images/official/nginx.png",
-- 	"helpurl": "https://hub.docker.com/_/nginx/",
-- 	"description":"Official build of Nginx."
--     }
-- }

-- in ghci

-- Object (fromList [("thumb",String "https://hub.docker.com/public/images/official/nginx.png"),("helpurl",String "https://hub.docker.com/_/nginx/"),("description",String "Official build of Nginx.")])
-- Object (fromList [("thumb",String "https://hub.docker.com/public/images/official/elasticsearch.png"),("helpurl",String "https://hub.docker.com/_/elasticsearch/"),("description",String "Elasticsearch is a powerful open source search and analytics engine that makes data easy to explore.")])


module Main where

import Control.Lens
import Data.Monoid
import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens
import System.Environment
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Exception as E
import System.FilePath.Posix
import System.Directory
import Network.Wreq
import Network.URI
default (T.Text)

traitementThumb :: String -> T.Text -> IO ()
traitementThumb dir t =
  if isURI $ T.unpack t
  then do
    putStrLn $ "Traitement de " <> T.unpack t
    r <- get $ T.unpack t
    BL.writeFile (dir <> (takeFileName (T.unpack t))) (r ^. responseBody)
  else
    putStrLn $ T.unpack t <> " n'est pas une URI ..."

main :: IO ()
main = do
  args <- getArgs
  case args of
    [config, path] -> do
      p <- doesDirectoryExist path
      js <- BL.readFile(config)
      if not p then
        createDirectory path
      else
        return ()
      traverseOf_ (members . key "thumb" . _String) (traitementThumb path) js
      putStrLn "Fin des traitements"
    _ -> putStrLn usage
  where
    usage = "prog config path"
