{-# OPTIONS_GHC -threaded        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import           Prelude hiding (FilePath)
import qualified Prelude as P
import           Turtle as T hiding (f,e,err)
import qualified Turtle.Bytes as TB
import qualified Control.Foldl as F
import           GHC.Generics
import           Control.Lens
import           Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Csv as CSV
import           Data.Text as Tx
import qualified Data.Text.Encoding as Tx
import           Data.Maybe
import           Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Network.Wreq as W

import           Debug.Trace

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
  , license :: Maybe License
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

data Issue
  = Issue
  { issue_title :: Text
  , issue_number :: Int
  , issue_state :: Text
  , issue_comments :: Int
  , issue_created_at :: Text
  , issue_updated_at :: Text
  , issue_closed_at :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON Issue where
  parseJSON = withObject "Issue" $
    \v -> Issue
            <$> v .: "title"
            <*> v .: "number"
            <*> v .: "state"
            <*> v .: "comments"
            <*> v .: "created_at"
            <*> v .: "updated_at"
            <*> v .:? "closed_at"
instance ToJSON Issue where
    toEncoding = genericToEncoding defaultOptions

data Pull
  = Pull
  { pull_title :: Text
  , pull_number :: Int
  , pull_state :: Text
  , pull_created_at :: Text
  , pull_updated_at :: Text
  , pull_closed_at :: Maybe Text
  , pull_merged_at :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON Pull where
  parseJSON = withObject "Pull" $
    \v -> Pull
            <$> v .: "title"
            <*> v .: "number"
            <*> v .: "state"
            <*> v .: "created_at"
            <*> v .: "updated_at"
            <*> v .:? "closed_at"
            <*> v .:? "merged_at"
instance ToJSON Pull where
    toEncoding = genericToEncoding defaultOptions

data GitLogLine
  = GitLogLine
  { gll_hash :: Text
  , gll_author :: Text
  , gll_author_email :: Text
  , gll_author_at :: Text
  , gll_commiter :: Text
  , gll_commiter_email :: Text
  , gll_commiter_at :: Text
  , gll_message :: Text
  } deriving (Show, Generic)
instance CSV.FromRecord GitLogLine where
  parseRecord v | V.length v >= 7 = GitLogLine <$> v `CSV.index` 0
                                               <*> v `CSV.index` 1
                                               <*> v `CSV.index` 2
                                               <*> v `CSV.index` 3
                                               <*> v `CSV.index` 4
                                               <*> v `CSV.index` 5
                                               <*> v `CSV.index` 6
                                               <*> v `CSV.index` 7
                | otherwise       = mzero

data Person
  = Person
  { person_name :: Text
  , person_email :: Text
  } deriving (Show, Generic)
instance ToJSON Person where
    toEncoding = genericToEncoding defaultOptions

data Commit
  = Commit
  { commit_message :: Text
  , commit_hash :: Text
  , commit_author :: Person
  , commit_commiter :: Person
  , commit_repository :: Text
  } deriving (Show, Generic)
instance ToJSON Commit where
    toEncoding = genericToEncoding defaultOptions

data CommitStats
  = CommitStats
  { cm_name :: [Text]
  , cm_commits :: Int
  , cm_authors :: Int
  } deriving (Show, Generic)
instance ToJSON CommitStats where
    toEncoding = genericToEncoding defaultOptions

data FileReport
  = FileReport
  { freport_files :: [Text]
  , freport_hasContributing :: Bool
  , freport_hasLicense :: Bool
  , freport_hasNotice :: Bool
  , freport_hasReadme :: Bool
  } deriving (Show, Generic)
instance ToJSON FileReport where
    toEncoding = genericToEncoding defaultOptions

data Report
  = Report
  { report_repo :: Repo
  , report_commits :: [Commit]
  , report_commitStats :: Map.Map Text CommitStats
  , report_issues :: [Issue]
  , report_pulls :: [Pull]
  , report_file_report :: FileReport
  } deriving (Show, Generic)
instance ToJSON Report where
    toEncoding = genericToEncoding defaultOptions

-- TODO: paging
reposUrl :: String -> String
reposUrl org = "https://api.github.com/orgs/" ++ org ++ "/repos"
issuesUrl, pullsUrl :: Repo -> String
issuesUrl repo = "https://api.github.com/repos/" ++ Tx.unpack (full_name repo) ++ "/issues"
pullsUrl repo = "https://api.github.com/repos/" ++ Tx.unpack (full_name repo) ++ "/pulls"

getHTTP :: FromJSON b => (a -> String) -> W.Options -> a -> IO (Either String b)
getHTTP urlGetter opts val = do
  let url = (urlGetter val)
  putStrLn ("get from: " ++ url)
  response :: (W.Response b) <- W.asJSON =<< W.getWith opts url
  let rCode = response ^. W.responseStatus . W.statusCode
  return $ if rCode == 200
           then Right (response ^. W.responseBody)
           else Left ("HTTP failed with ERR code " ++ show rCode)

getReposHTTP :: W.Options -> String -> IO (Either String [Repo])
getReposHTTP = getHTTP reposUrl

getIssuesHTTP :: W.Options -> Repo -> IO (Either String [Issue])
getIssuesHTTP = getHTTP issuesUrl

getPullsHTTP :: W.Options -> Repo -> IO (Either String [Pull])
getPullsHTTP = getHTTP pullsUrl

cloneIfNecessary :: Repo -> IO ()
cloneIfNecessary Repo{full_name = f, clone_url = c} = do
  let target = fromString $ Tx.unpack f
  exists <- testdir target
  unless exists $ do
    putStrLn ("### clone from: " ++ (show c))
    mktree target
    e <- T.proc "git" ["clone", c, f] (T.select [])
    print e

handleCommitsOfRepo :: Repo -> IO (Either String [Commit])
handleCommitsOfRepo repo = let
    -- TODO: ugly:
    input = TB.inproc "git" ["log", "--date=iso", "--pretty=format:\"%h\",\"%an\",\"%ae\",\"%ad\",\"%cn\",\"%ce\",\"%cd\",\"%s\""] (T.select [])
    inputWOMsg = TB.inproc "git" ["log", "--date=iso", "--pretty=format:\"%h\",\"%an\",\"%ae\",\"%ad\",\"%cn\",\"%ce\",\"%cd\",\"\""] (T.select [])
    handleGitLogLine :: GitLogLine -> Commit
    handleGitLogLine gll = Commit (gll_message gll)
                                  (gll_hash gll)
                                  (Person (gll_author gll) (gll_author_email gll))
                                  (Person (gll_commiter gll) (gll_commiter_email gll))
                                  (full_name repo)
  in do
    csvData <- fmap BSL.fromStrict $ fold (input) (F.foldMap id id)
    case (CSV.decode CSV.NoHeader csvData) :: Either String (V.Vector GitLogLine) of
      Left err1 -> do
        print err1
        csvDataWOMsg <- fmap BSL.fromStrict $ fold (inputWOMsg) (F.foldMap id id)
        return $ case (CSV.decode CSV.NoHeader csvDataWOMsg) :: Either String (V.Vector GitLogLine) of
          Left err2 -> Left err2
          Right results -> Right (V.toList (V.map handleGitLogLine results))
      Right results -> return $ Right (V.toList (V.map handleGitLogLine results))

handleFilesOfRepo :: Repo -> IO FileReport
handleFilesOfRepo repo = do
  lines <- fold (T.inproc "git" ["ls-files"] (T.select [])) F.list
  let files = (L.map lineToText lines)
  let funToTestFiles = \s -> L.any (s `Tx.isPrefixOf`) files
  let hasContributing = funToTestFiles "CONTRIBUTING"
      hasLicense = funToTestFiles "LICENSE"
      hasNotice = funToTestFiles "NOTICE"
      hasReadme = funToTestFiles "README"
  return (FileReport files hasContributing hasLicense hasNotice hasReadme)

computeUserStats :: [Commit] -> Map.Map Text CommitStats
computeUserStats commits = let
    stupidMapStats :: Commit -> [(Text, CommitStats)]
    stupidMapStats commit = let
        author = commit_author commit
        commiter = commit_commiter commit
      in [ (person_email commiter, CommitStats [person_name commiter] 1 0)
         , (person_email author, CommitStats [person_name author] 0 1) ]
    joinStats s1 s2 = CommitStats (L.nub $ cm_name s1 ++ cm_name s2) (cm_commits s1 + cm_commits s2) (cm_authors s1 + cm_authors s2)
  in Map.fromListWith joinStats $ L.concatMap stupidMapStats commits

handleRepo :: W.Options -> Repo -> IO Report
handleRepo opts repo = let
    handleEitherResult :: Either String [a] -> IO [a]
    handleEitherResult (Left err) = do
      print err
      return []
    handleEitherResult (Right vals) = return vals
  in do
    putStrLn ("### handle repo: " ++ (show (full_name repo)))
    cloneIfNecessary repo
    T.cd $ T.fromText (full_name repo)

    commits <- handleEitherResult =<< handleCommitsOfRepo repo

    let userStats = computeUserStats commits

    issues <- handleEitherResult =<< getIssuesHTTP opts repo
    pulls <- handleEitherResult =<< getPullsHTTP opts repo

    fileReport <- handleFilesOfRepo repo

    return (Report repo commits userStats issues pulls fileReport)

handleReport :: Report -> IO ()
handleReport report@(Report{report_repo = repo@Repo{full_name = fn, name = n}}) = do
  putStrLn ("### write reports of: " ++ (show (full_name repo)))

  let pReport = A.encodePretty report
      reportPath = T.fromText $ fn `Tx.append` (Tx.pack ".json")
  TB.output reportPath (return (BSL.toStrict pReport))

  let emailsCsvPath = T.fromText $ fn `Tx.append` (Tx.pack "_emails.csv")
      emails = ( Tx.encodeUtf8
               . Tx.unlines
               . ("email,#authors,#commits,names...":)
               . L.map (Tx.intercalate (singleton ','))
               . L.map (\(k,v) -> (k
                                   : ((Tx.pack . show $ cm_authors v)
                                      : ((Tx.pack . show $ cm_commits v)
                                         : (cm_name v)))))
               . Map.assocs
               . report_commitStats) report
  TB.output emailsCsvPath (return emails)

main :: IO ()
main = let
    optionsParser :: T.Parser (BS.ByteString, BS.ByteString, String, FilePath)
    optionsParser = (,,,) <$> (fmap Tx.encodeUtf8 $ argText "user" "User")
                          <*> (fmap Tx.encodeUtf8 $ argText "token" "Token")
                          <*> (fmap Tx.unpack $ argText "org" "Org")
                          <*> argPath "target" "Target"
  in do
    (user, token, org, target) <- options "repoScan" optionsParser
    let opts = W.defaults & W.auth ?~ W.basicAuth user token

    T.mktree target
    T.cd target

    root <- T.pwd

    parsed <- getReposHTTP opts org
    case parsed of
      Left err    -> print err
      Right repos -> do
        mapM_ (\repo -> T.cd root >> handleRepo opts repo >>= (\report -> T.cd root >> handleReport report)) repos