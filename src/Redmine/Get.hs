{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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
                   , MaybeIO
                   , runMaybeIO
                   ) where

import Data.Aeson
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map
import Data.Tuple
import Redmine.Types
import Redmine.Manager
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import System.Time
import Network
import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS
import Network.HTTP.Client (defaultManagerSettings)
import Data.Time.Format     (parseTime)
import Data.Time.Clock      (UTCTime)
import System.Locale        (defaultTimeLocale)
import Control.Monad        (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Maybe
import Control.Monad.Trans.Resource
--import GHC.Generics
import Debug.Trace

-- FIXME : Change IO Maybe to MaybeIO
newtype MaybeIO a = MaybeIO {
   runMaybeIO :: IO (Maybe a)
}

instance Functor MaybeIO where
    fmap f (MaybeIO a) = MaybeIO $ fmap (fmap f) a

instance Monad MaybeIO where
    return = MaybeIO . return . return
    a' >>= b' = MaybeIO $ a >>= maybe (return Nothing) ((>>= b) . return)
        where a = runMaybeIO a'
              b = runMaybeIO . b'

--instance MonadIO MaybeIO where
--    liftIO x = MaybeIO $ x >>= return . Just

parseRHTime :: String -> Maybe UTCTime
parseRHTime = parseTime defaultTimeLocale "%FT%X%QZ"

parseShortTime :: String -> Maybe UTCTime
parseShortTime = parseTime defaultTimeLocale "%F"

queryRedmine :: RedmineMng -> S.ByteString -> IO L.ByteString
queryRedmine mng req = do
          request <- creerRqt mng req
          let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
          response  <- withManagerSettings settings $ httpLbs request
          return $ responseBody response
          --withManager $ \manager -> do
          --    response <- httpLbs request manager
          --    return $ responseBody response

creerRqt :: RedmineMng -> S.ByteString -> IO Request
creerRqt (RedmineMng h) r                            = parseUrl $ S8.unpack (h `S.append` r)
creerRqt (RedmineMngWithProxy h u p) r               = fmap (addProxy u p) (creerRqt (RedmineMng h) r)
creerRqt (RedmineMngWithAuth h l pass) r             = fmap (applyBasicAuth l pass) (creerRqt (RedmineMng h) r)
creerRqt (RedmineMngWithAuthAndProxy h l pass u p) r = fmap (applyBasicAuth l pass) (creerRqt (RedmineMngWithProxy h u p) r)

type ParamRest = Map.Map S.ByteString S.ByteString

-- Remplace par urlEncodedBody
expandOptions :: ParamRest -> S.ByteString
expandOptions = Map.foldrWithKey (\k a res -> res `S.append` k `S.append` "=" `S.append` a `S.append` "&") "?"

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
queryRedmineAvecOptions redmineMng req param mng = -- MaybeIO $ withSocketsDo $
  do
    request   <- creerRqt redmineMng (req `S.append` (expandOptions param))
    --traceM (S8.unpack $ (req `S.append` (expandOptions param)))
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing --(SockSettingsSimple "www-only-proxy.services.boeing.com" 31061)
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
  traceIO $ show toto
  (debugResult . eitherDecode) $ toto

initOpt = Map.fromList [("offset","0"), ("limit","100")]

-- |The function 'getTimeEntries' fetch all the time entries.
getTimeEntries :: RedmineMng -> MaybeIO [TimeEntry]
getTimeEntries mng = MaybeIO $ do
   mngConn <- newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng requete initOpt mngConn
   return $ fmap time_entries res
   where requete = "/time_entries.json"

getTimeEntriesForIssue :: RedmineMng -> Integer-> MaybeIO [TimeEntry]
getTimeEntriesForIssue mng issueid = MaybeIO $ do
   mngConn <- liftIO $ newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng requete initOpt mngConn
   return $ fmap time_entries res
   where requete = "/time_entries/" `S.append` (S8.pack $ show issueid) `S.append` ".json"

getIssues :: RedmineMng -> ParamRest -> MaybeIO [Issue]
getIssues mng param = MaybeIO $ do
   mngConn <- liftIO $ newManager tlsManagerSettings
   --putStrLn $ S8.unpack requete
   res <- queryRedmineAvecOptions mng requete ( Map.union param initOpt) mngConn
   return $ fmap issues res
   where requete = "/issues.json"

getIssue :: RedmineMng -> Integer -> ParamRest -> MaybeIO Issue
getIssue mng nb param = do
   --traceM $ S8.unpack requete
   fmap issue (MaybeIO $ runQuery mng requete)
   where requete = "/issues/" `S.append` (S8.pack $ show nb) `S.append` ".json" `S.append` (expandOptions param)

getProjects :: RedmineMng -> MaybeIO [Project]
getProjects mng = MaybeIO $ do
   mngConn <- liftIO $ newManager tlsManagerSettings
   res <- queryRedmineAvecOptions mng requete initOpt mngConn
   return $ fmap projects res
   where requete = "/projects.json"

getProjectForId :: RedmineMng -> Integer -> MaybeIO Project
getProjectForId mng projId = do
   MaybeIO $ runQuery mng requete
   where requete = (rmhost mng) `S.append` "/projects/" `S.append` (S8.pack $ show projId) `S.append` ".json"

getProject :: RedmineMng -> S.ByteString -> MaybeIO Project
getProject mng projId = do
   MaybeIO $ runQuery mng requete
   where requete = "/projects/" `S.append` projId `S.append` ".json"

--Get all the versions associated to a project
getVersions :: RedmineMng --The connection manager
            -> S.ByteString --The project
            -> MaybeIO [Version]
getVersions mng proj = do
   fmap versions (MaybeIO $ runQuery mng requete)
   where requete = "/projects/" `S.append` proj `S.append` "/versions.json"

getVersion:: RedmineMng -> Integer -> ParamRest -> MaybeIO Version
getVersion mng nb param = do
   --traceM $ S8.unpack requete
   fmap version (MaybeIO $ runQuery mng requete)
   where requete = "/versions/" `S.append` (S8.pack $ show nb) `S.append` ".json" `S.append` (expandOptions param)

getUser :: RedmineMng -> Integer -> MaybeIO User
getUser mng id =
  do let requete = "/users/" `S.append` (S8.pack $ show id) `S.append` ".json"
     --putStrLn . S8.unpack $ requete
     MaybeIO $ runQuery mng requete
    --where requete = "/users/" `S.append` (S8.pack $ show id) `S.append` ".json"

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
              --FIXME avoid parsing an empty string
              <*> liftM (parseShortTime . fromMaybe "") (v .:? "spent_on")

instance FromJSON VersionsRsp where
  parseJSON (Object v) = VersionsRsp <$> (v .: "versions")

instance FromJSON VersionRsp where
  parseJSON (Object v) = VersionRsp <$> (v .: "version")

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

