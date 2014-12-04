{-# LANGUAGE OverloadedStrings #-}

module Redmine.Get ( getTimeEntries
                   , getTimeEntriesForIssue
                   , getIssue
                   , getIssues
                   , getProjects
                   , getProjectForId
                   , getProject
                   , getVersions
                   , getUser
                   , expandOptions
                   , increaseQueryRange
                   ) where

import Data.Aeson
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map
import Data.Tuple
import Redmine.Types
import Redmine.Manager
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Network
import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS
import Network.HTTP.Client.Conduit (defaultManagerSettings)
import Data.Time.Format     (parseTime)
import Data.Time.Clock      (UTCTime)
import Data.Time.Calendar (Day)
import System.Locale        (defaultTimeLocale)
import Control.Monad        (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Data.String.Utils
import Debug.Trace
import qualified Data.Text as T

parseRHTime :: String -> Maybe UTCTime
parseRHTime = parseTime defaultTimeLocale "%FT%X%QZ"

parseShortTime :: String -> Maybe Day
parseShortTime = parseTime defaultTimeLocale "%F"

queryRedmine :: RedmineMng -> S.ByteString -> IO L.ByteString
queryRedmine mng req = do
          request <- creerRqt mng req
          let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
          response  <- withManagerSettings settings $ httpLbs request
          return $ responseBody response

creerRqt :: RedmineMng -> S.ByteString -> IO Request
creerRqt (RedmineMng h) r                            = parseUrl $ S8.unpack (h <> r)
creerRqt (RedmineMngWithProxy h u p) r               = fmap (addProxy u p) (creerRqt (RedmineMng h) r)
creerRqt (RedmineMngWithAuth h l pass) r             = fmap (applyBasicAuth l pass) (creerRqt (RedmineMng h) r)
creerRqt (RedmineMngWithAuthAndProxy h l pass u p) r = fmap (applyBasicAuth l pass) (creerRqt (RedmineMngWithProxy h u p) r)

type ParamRest = Map.Map S.ByteString S.ByteString

-- Remplace par urlEncodedBody
expandOptions :: ParamRest -> S.ByteString
expandOptions = Map.foldrWithKey (\k a res -> res <> k <> "=" <> a <> "&") "?"

bsAInt :: S.ByteString -> Int
bsAInt = read . S8.unpack

increaseQueryRange :: ParamRest -> ParamRest
increaseQueryRange param =
  let offset = bsAInt $ Map.findWithDefault "0" "offset" param
      limit  = bsAInt $ Map.findWithDefault "100" "limit" param
      nouvelOffset = offset + limit
  in Map.insert "offset" (S8.pack $ show nouvelOffset) param

-- Réécrire avec les autres modes
queryRedmineAvecOptions :: (FromJSON a, Monoid a, Collection a) =>
                           RedmineMng -> S.ByteString -> ParamRest -> Manager -> IO( Maybe a)
queryRedmineAvecOptions redmineMng req param mng = -- MaybeT IO $ withSocketsDo $
  do
    request   <- creerRqt redmineMng (req <> expandOptions param)
    --traceM (S8.unpack $ (req <> (expandOptions param)))
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    response  <- withManagerSettings settings $ httpLbs request
    parsedRes <- debugResult . eitherDecode . responseBody $ response
    --putStrLn $ show parsedRes
    case parsedRes of
      Just a | 0 == longueur a -> return Nothing
             | otherwise       ->
                do let hausse = increaseQueryRange param
                   --traceM . show $ hausse
                   reste <- queryRedmineAvecOptions redmineMng req hausse mng
                   case reste of
                      Just b -> return . Just $ mappend a b
                      Nothing -> return $ Just a
                   --return (reste >>= (\b -> Just $ mappend a b ))

      Nothing -> return Nothing

-- |The function 'debugResult' is used to print the parsing error statement
-- and continue the processing.
debugResult :: Either String a -> IO(Maybe a)
debugResult res = case res of
                    Left msg -> putStrLn msg >> return Nothing
                    Right v  -> return (Just v)

runQuery :: FromJSON a => RedmineMng -> S.ByteString -> IO( Maybe a)
runQuery mng requete = do -- withSocketsDo $ do
  toto <- queryRedmine mng requete
  (debugResult . eitherDecode) toto

initOpt = Map.fromList [("offset","0"), ("limit","100")]

-- |The function 'getTimeEntries' fetch all the time entries.
-- |They can be filtered by spenton date using spent_on=%3E%3C2013-05-01|2013-05-31
getTimeEntries :: RedmineMng -> ParamRest -> MaybeT IO [TimeEntry]
getTimeEntries mng param = MaybeT $ do
   mngConn <- newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng requete ( Map.union param initOpt) mngConn
   return $ fmap time_entries res
   where requete = "/time_entries.json"

getTimeEntriesForIssue :: RedmineMng -> Integer-> MaybeT IO [TimeEntry]
getTimeEntriesForIssue mng issueid = MaybeT $ do
   mngConn <- newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng requete initOpt mngConn
   return $ fmap time_entries res
   where requete = "/time_entries/" <> S8.pack (show issueid) <> ".json"

getIssues :: RedmineMng -> ParamRest -> MaybeT IO [Issue]
getIssues mng param = MaybeT $ do
   mngConn <- liftIO $ newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng requete ( Map.union param initOpt) mngConn
   return $ fmap issues res
   where requete = "/issues.json"

getIssue :: RedmineMng -> Integer -> ParamRest -> MaybeT IO Issue
getIssue mng elemId param =
   fmap issue (MaybeT $ runQuery mng requete)
   where requete = "/issues/" <> S8.pack (show elemId) <> ".json" <> expandOptions param

getProjects :: RedmineMng -> MaybeT IO [Project]
getProjects mng = MaybeT $ do
   mngConn <- newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng requete initOpt mngConn
   return $ fmap projects res
   where requete = "/projects.json"

getProjectForId :: RedmineMng -> Integer -> MaybeT IO Project
getProjectForId mng elemId =
   MaybeT $ runQuery mng requete
   where requete = rmhost mng <> "/projects/" <> S8.pack (show elemId) <> ".json"

getProject :: RedmineMng -> S.ByteString -> MaybeT IO Project
getProject mng projId =
   MaybeT $ runQuery mng requete
   where requete = "/projects/" <> projId <> ".json"

--Get all the versions associated to a project
getVersions :: RedmineMng --The connection manager
            -> S.ByteString --The project
            -> MaybeT IO [Version]
getVersions mng proj =
   fmap versions (MaybeT $ runQuery mng requete)
   where requete = "/projects/" <> proj <> "/versions.json"

getVersion:: RedmineMng -> Integer -> ParamRest -> MaybeT IO Version
getVersion mng elemId param =
   fmap version (MaybeT $ runQuery mng requete)
   where requete = "/versions/" <> S8.pack (show elemId) <> ".json" <> expandOptions param

getUser :: RedmineMng -> Integer -> MaybeT IO User
getUser mng elemId =
  do let requete = "/users/" <> S8.pack (show elemId) <> ".json"
     MaybeT $ runQuery mng requete

instance FromJSON ObjRef where
  parseJSON (Object v) =
    ObjRef <$> (v .: "id")
           <*> (v .: "name")

instance FromJSON ObjID where
  parseJSON (Object v) =
    ObjID <$> (v .: "id")

instance FromJSON IssuesRsp where
  parseJSON (Object v) = IssuesRsp <$> (v .: "issues")

instance FromJSON IssueRsp where
  parseJSON (Object v) = IssueRsp <$> (v .: "issue")

instance FromJSON Issue where
  parseJSON (Object v) =
    Issue <$> (v .: "id")
          <*> (v .: "project")
          <*> (v .:? "parent")
          <*> (v .: "tracker")
          <*> (v .: "status")
          <*> (v .: "priority")
          <*> (v .: "author")
          <*> (v .:? "assigned_to")
          <*> (v .:? "category")
          <*> (v .: "fixed_version")
          <*> (v .: "subject")
          <*> (v .: "description")
          <*> liftM (parseShortTime . fromMaybe "") (v .:? "start_date")
          <*> liftM (parseShortTime . fromMaybe "") (v .:? "due_date")
          <*> (v .: "done_ratio")
          <*> (v .:? "estimated_hours")
          <*> (v .:? "spent_hours")
          <*> (v .:? "custom_fields")
          <*> liftM parseRHTime (v .: "created_on")
          <*> liftM parseRHTime (v .: "updated_on")
          <*> (v .:? "journals")
          <*> (v .:? "attachements")
          <*> (v .:? "changesets")
          <*> (v .:? "watchers")
          <*> (v .:? "relations")
          <*> (v .:? "children")


instance FromJSON Child where
  parseJSON (Object v) =
    Child <$> (v .: "id")
          <*> (v .: "tracker")
          <*> (v .: "subject")

instance FromJSON Attachement where
  parseJSON (Object v) =
    Attachement <$> (v .: "id")
                <*> (v .: "filename")
                <*> (v .: "filesize")
                <*> (v .: "content_type")
                <*> (v .: "description")
                <*> (v .: "content_url")
                <*> (v .: "author_name")
                <*> liftM (fromJust.parseRHTime) (v .: "created_on")

instance FromJSON ChangeSet where
  parseJSON (Object v) =
    ChangeSet <$> (v .: "revision")
              <*> (v .: "user")
              <*> (v .: "comments")
              <*> liftM (fromJust.parseRHTime) (v .: "committed_on")

instance FromJSON Watcher where
  parseJSON (Object v) =
    Watcher <$> (v .: "id")
            <*> (v .: "name")

instance FromJSON CustomField where
  parseJSON (Object v) =
    CustomField <$> (v .: "id") <*> (v .: "name") <*> (v .: "value")

instance FromJSON Journal where
  parseJSON (Object v) =
    Journal <$> (v .: "id")
            <*> (v .: "user")
            <*> (v .:? "notes" .!= "")
            <*> liftM parseRHTime (v .: "created_on")
            <*> (v .: "details")

instance FromJSON Detail where
  parseJSON (Object v) =
    Detail  <$> (v .: "property")
            <*> (v .: "name")
            <*> (v .:? "old_value")
            <*> (v .: "new_value")

instance FromJSON ProjectsRsp where
  parseJSON (Object v) = ProjectsRsp <$> (v .: "projects")

instance FromJSON Project where
  parseJSON (Object v) =
    Project <$> (v .: "id")
            <*> (v .: "name")
            <*> (v .: "identifier")
            <*> (v .: "description")
            <*> (v .:? "custom_fields")
            <*> liftM parseRHTime (v .: "created_on")
            <*> liftM parseRHTime (v .: "updated_on")

instance FromJSON TimeEntriesRsp where
  parseJSON (Object v) = TimeEntriesRsp <$> (v .: "time_entries")

instance FromJSON TimeEntry where
  parseJSON (Object v) =
    TimeEntry <$> (v .: "id")
              <*> (v .: "project")
              <*> (v .: "issue")
              <*> (v .: "user")
              <*> (v .:? "activity")
              <*> (v .:? "hours")
              <*> (v .: "comments")
              <*> liftM parseRHTime (v .: "created_on")
              <*> liftM parseRHTime (v .: "updated_on")
              <*> liftM (parseShortTime . fromMaybe "") (v .:? "spent_on")

instance FromJSON VersionsRsp where
  parseJSON (Object v) = VersionsRsp <$> (v .: "versions")

instance FromJSON VersionRsp where
  parseJSON (Object v) = VersionRsp <$> (v .: "version")

instance FromJSON Day where
    parseJSON = withText "Day" $ \t ->
        case parseTime defaultTimeLocale "%F" (T.unpack t) of
          Just d -> pure d
          _      -> fail "could not parse ISO-8601 date"

instance FromJSON Version where
  parseJSON (Object v) =
    Version <$> (v .: "id")
            <*> (v .: "name")
            <*> (v .: "project")
            <*> (v .: "description")
            <*> (v .: "status")
            <*> (v .: "sharing")
            --FIXME avoid parsing an empty string
            <*> liftM (parseShortTime . fromMaybe "") (v .:? "due_date")
            <*> liftM parseRHTime (v .: "created_on")
            <*> liftM parseRHTime (v .: "updated_on")

instance FromJSON Relations where
  parseJSON (Object v) = Relations <$> (v .: "relations")

instance FromJSON Relation where
  parseJSON (Object v) =
    Relation <$> (v .: "id")
             <*> (v .: "issue_id")
             <*> (v .: "issue_to_id")
             <*> (v .: "relation_type")
             <*> (v .:? "delay")

instance FromJSON Roles where
  parseJSON (Object v) = Roles <$> (v .: "roles")

instance FromJSON Role where
  parseJSON (Object v) =
    Role <$> (v .: "id") <*> (v .: "name")

instance FromJSON Memberships where
  parseJSON (Object v) = Memberships <$> (v .: "memberships")

instance FromJSON Membership where
  parseJSON (Object v) =
    Membership <$> (v .: "id")
               <*> (v .: "project")
               <*> (v .: "user")
               <*> (v .: "roles")

instance FromJSON UsersRsp where
  parseJSON (Object v) = UsersRsp <$> (v .: "users")

instance FromJSON User where
  parseJSON (Object v) =
    User <$> (v .: "lastname")
         <*> liftM parseRHTime (v .: "created_on")
         <*> (v .: "mail")
         <*> liftM parseRHTime (v .: "last_login_on")
         <*> (v .: "firstname")
         <*> (v .: "id")

instance FromJSON Trackers where
  parseJSON (Object v) = Trackers <$> (v .: "trackers")

instance FromJSON Tracker where
  parseJSON (Object v) =
    Tracker <$> (v .: "id") <*> (v .: "name")

instance FromJSON IssueStatuses where
  parseJSON (Object v) = IssueStatuses <$> (v .: "issue_statuses")

instance FromJSON IssueStatus where
  parseJSON (Object v) =
    IssueStatus <$> (v .: "id")
                <*> (v .: "name")
                <*> (v .: "is_default")
                <*> (v .: "is_closed")

