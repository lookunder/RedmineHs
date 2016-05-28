{-# LANGUAGE OverloadedStrings #-}

module Redmine.JSON
( parseJSON
, toJSON
) where

import Data.Aeson
import Data.Maybe
import Redmine.Types
import Control.Applicative ((<$>), (<*>), pure)
import Data.Time (Day, UTCTime, showGregorian, defaultTimeLocale)
import Data.Time.Format (parseTimeM)

import qualified Data.Text  as T (pack, unpack)

parseRHTime :: String -> Maybe UTCTime
parseRHTime = parseTimeM True defaultTimeLocale "%FT%X%QZ"

parseShortTime :: String -> Maybe Day
parseShortTime = parseTimeM True defaultTimeLocale "%F"

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
          <*> (v .:? "fixed_version")
          <*> (v .: "subject")
          <*> (v .:? "description" .!= "")
          <*> fmap (parseShortTime . fromMaybe "") (v .:? "start_date")
          <*> fmap (parseShortTime . fromMaybe "") (v .:? "due_date")
          <*> (v .: "done_ratio")
          <*> (v .:? "estimated_hours")
          <*> (v .:? "spent_hours")
          <*> (v .:? "custom_fields")
          <*> fmap parseRHTime (v .: "created_on")
          <*> fmap parseRHTime (v .: "updated_on")
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
                <*> fmap (fromJust.parseRHTime) (v .: "created_on")

instance FromJSON ChangeSet where
  parseJSON (Object v) =
    ChangeSet <$> (v .: "revision")
              <*> (v .: "user")
              <*> (v .: "comments")
              <*> fmap (fromJust.parseRHTime) (v .: "committed_on")

instance FromJSON Watcher where
  parseJSON (Object v) =
    Watcher <$> (v .: "id")
            <*> (v .: "name")

instance FromJSON CustomField where
  parseJSON (Object v) =
    CustomField <$> (v .: "id")
                <*> (v .: "name")
                <*> (v .:? "value" .!= "")

instance FromJSON Journal where
  parseJSON (Object v) =
    Journal <$> (v .: "id")
            <*> (v .: "user")
            <*> (v .:? "notes" .!= "")
            <*> fmap parseRHTime (v .: "created_on")
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
            <*> fmap parseRHTime (v .: "created_on")
            <*> fmap parseRHTime (v .: "updated_on")

instance FromJSON TimeEntriesRsp where
  parseJSON (Object v) = TimeEntriesRsp <$> (v .: "time_entries")

instance FromJSON TimeEntryRsp where
  parseJSON (Object v) = TimeEntryRsp <$> (v .: "time_entry")

instance FromJSON TimeEntry where
  parseJSON (Object v) =
    TimeEntry <$> (v .: "id")
              <*> (v .: "project")
              <*> (v .: "issue")
              <*> (v .: "user")
              <*> (v .:? "activity")
              <*> (v .:? "hours")
              <*> (v .: "comments")
              <*> fmap parseRHTime (v .: "created_on")
              <*> fmap parseRHTime (v .: "updated_on")
              <*> fmap (parseShortTime . fromMaybe "") (v .:? "spent_on")

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
            <*> fmap (parseShortTime . fromMaybe "") (v .:? "due_date")
            <*> fmap parseRHTime (v .: "created_on")
            <*> fmap parseRHTime (v .: "updated_on")

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

instance FromJSON UserRsp where
  parseJSON (Object v) = UserRsp <$> (v .: "user")

instance FromJSON User where
  parseJSON (Object v) =
    User <$> (v .: "lastname")
         <*> fmap parseRHTime (v .: "created_on")
         <*> (v .: "mail")
         <*> fmap parseRHTime (v .: "last_login_on")
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

instance ToJSON ObjRef where
  toJSON (ObjRef id name) = object [ "id" .= id, "name" .= name]

instance ToJSON ObjID where
  toJSON (ObjID id) = object ["id" .= id]

instance ToJSON Issue where
  toJSON (Issue _id'
          project
          _parent
          _tracker
          _status
          _priority
          _author
          _assigned_to
          _category
          _fixed_version
          subject
          description
          _start_date
          _due_date
          _done_ratio
          _estimated_hours
          _spent_hours
          _custom_fields
          _created_on
          _updated_on
          _journals
          _attachements
          _changesets
          _watchers
          _relations
          _children
         ) =    object  [ "issue" .=
    object [ -- "id" .= id',
             "project_id" .= id_ObjRef project
--           , "tracker" .= tracker
--           , "status" .= status
--           , "priority" .= priority
--           , "author"   .= author
--           , "assigned_to" .= assigned_to
--           , "category" .= category
--           , "fixed_version" .= fixed_version
           , "subject" .= subject
           , "description" .= description
--           , "start_date" .= start_date
--           , "due_date" .= due_date
--           , "done_ratio" .= done_ratio
--           , "estimated_hours" .= estimated_hours
--           , "spent_hours" .= spent_hours
--           , "custom_fields" .= custom_fields
--           , "created_on" .= created_on
--           , "updated_on" .= updated_on
--           , "journals" .= journals
--           , "attachements" .= attachements
--           , "changesets" .= changesets
--           , "watchers" .= watchers
--           , "relations" .= relations
--           , "children" .= children
           ]]

instance ToJSON Child where
  toJSON (Child id' tracker subject) =
    object [ "id" .= id'
           , "tracker" .= tracker
           , "subject" .= subject]

instance ToJSON Attachement where
  toJSON (Attachement id' filename filesize content_type description content_url author_name created_on) =
    object [ "id" .= id'
           , "filename" .= filename
           , "filesize" .= filesize
           , "content_type" .= content_type
           , "description" .= description
           , "content_url" .= content_url
           , "author_name" .= author_name
           , "created_on" .= created_on]

instance ToJSON ChangeSet where
  toJSON (ChangeSet revision user comments committed_on) =
    object [ "revision" .= revision
           , "user" .= user
           , "comments" .= comments
           , "commited_on" .= committed_on]


instance ToJSON CustomField where
  toJSON (CustomField id' name value) =
    object ["id" .= id'
           , "name" .= name
           , "value" .= value]

instance ToJSON Journal where
  toJSON (Journal id' user notes created_on details) =
    object ["id" .= id'
           , "user" .= user
           , "notes" .= notes
           , "created_on" .= created_on
           , "details" .= details]

instance ToJSON Detail where
  toJSON (Detail property name old_value new_value) =
    object [ "property" .= property
           , "name" .= name
           , "old_value" .= old_value
           , "new_value" .= new_value]

instance ToJSON Watcher where
  toJSON (Watcher id' name) =
    object [ "id" .= id'
           , "name" .= name]

instance ToJSON Relation where
  toJSON (Relation id' issue_id issue_to_id relation_type delay) =
    object [ "id" .= id'
           , "issue_id" .= issue_id
           , "issue_to_id" .= issue_to_id
           , "relation_type" .= relation_type
           , "delay" .= delay]

