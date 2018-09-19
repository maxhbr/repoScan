{-# OPTIONS_GHC -threaded        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import           Prelude hiding (FilePath)
import qualified Prelude as P
import           Turtle as T hiding (f)
import           GHC.Generics
import           Control.Lens
import           Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import           Data.Text as Tx
import           Data.Maybe
import           Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import qualified Network.Wreq as W

data License
  = License
  { license_key :: Text
  , license_name :: Text
  , license_spdx_id :: Maybe Text
  , license_url :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON License where
  parseJSON = withObject "License" $
    \v -> License
            <$> v .: "key"
            <*> v .: "name"
            <*> v .:? "spdx_id"
            <*> v .:? "url"
instance ToJSON License where
    toEncoding = genericToEncoding defaultOptions

data Repo
  = Repo
  { name :: Text
  , full_name :: Text
  , license :: (Maybe License)
  , fork :: Bool
  , archived :: Bool
  , created_at :: Text
  , updated_at :: Text
  , pushed_at :: Text
  , clone_url :: Text
  , git_url :: Text
  , ssh_url :: Text
  , size :: Int
  , open_issues_count :: Int
  , stargazers_count :: Int
  , watchers_count :: Int
  , forks_count :: Int
  } deriving (Show, Generic)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $
    \v -> Repo
            <$> v .: "name"
            <*> v .: "full_name"
            <*> v .:? "license"
            <*> v .: "fork"
            <*> v .: "archived"
            <*> v .: "created_at"
            <*> v .: "updated_at"
            <*> v .: "pushed_at"
            <*> v .: "clone_url"
            <*> v .: "git_url"
            <*> v .: "ssh_url"
            <*> v .: "size"
            <*> v .: "open_issues_count"
            <*> v .: "stargazers_count"
            <*> v .: "watchers_count"
            <*> v .: "forks_count"
instance ToJSON Repo where
    toEncoding = genericToEncoding defaultOptions

data Report
  = Report
  { report_repo :: Repo
  } deriving (Show, Generic)

instance ToJSON Report where
    toEncoding = genericToEncoding defaultOptions

-- TODO: paging
reposUrl org = "https://api.github.com/orgs/" ++ org ++ "/repos"
issuesUrl org repo = "https://api.github.com/repos/" ++ org ++ "/" ++ repo ++ "/issues"
pullsUrl org repo = "https://api.github.com/repos/" ++ org ++ "/" ++ repo ++ "/pulls"

getReposHTTP :: String -> IO (Either String [Repo])
getReposHTTP org = do
  response :: (W.Response [Repo]) <- W.asJSON =<< W.get (reposUrl org)
  let rCode = response ^. W.responseStatus . W.statusCode
  return $ if rCode == 200
           then Right (response ^. W.responseBody)
           else Left ("HTTP failed with ERR code " ++ (show rCode))

cloneIfNecessary :: Repo -> IO ()
cloneIfNecessary Repo{full_name = f, clone_url = c} = do
  let target = fromString $ Tx.unpack f
  exists <- testdir target
  unless exists $ do
    mktree target
    e <- T.proc "git" ["clone", c, f] (T.select [])
    print e

handleRepo :: Repo -> IO Report
handleRepo repo = do
  cloneIfNecessary repo
  return (Report repo)

handleReport :: Report -> IO ()
handleReport = BSL.putStrLn . A.encodePretty

main :: IO ()
main = let
    optionsParser :: T.Parser (Text, FilePath)
    optionsParser = (,) <$> argText "source" "Source"
                 <*> argPath "target" "Target"
  in do
    (org, target) <- options "Transformer" optionsParser
    T.mktree target
    T.cd target
    parsed <- getReposHTTP (Tx.unpack org)
    case parsed of
      Left error  -> print error
      Right repos -> do
        reports <- mapM handleRepo repos
        mapM_ handleReport reports
        print (L.length reports)
