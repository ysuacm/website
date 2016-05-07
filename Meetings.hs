{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Meetings where

import Data.Default
import qualified Data.Text as T
import Data.Thyme
import Data.Thyme.Format
import Data.Thyme.Time
import System.Locale

upcomingEvents :: [Meeting LocalTime]
upcomingEvents =
  [ Meeting
      "This Meeting Happened"
      "Ricky Elrod"
      (meetingAt "2016-04-20 17:30")
      def
      "This meeting already happened and should not appear."
      Nothing
  , Meeting
      "Configuration Driven Development"
      "Ricky Elrod"
      (meetingAt "2016-04-29 12:30")
      def
      "YSU Mathematics and Computer Science student Ricky Elrod will discuss \
      \techniques for dealing with safer, more descriptive configuration files \
      \that allow for better end-user customization of programs."
      (Just "/static/foo.pdf")
  , Meeting
      "An Introduction to Doing Things"
      "John Doe"
      (meetingAt "2016-05-13 17:30")
      def
      "Learn how to do a mix of things and stuff in this talk by Computer \
      \Information Systems student John Doe."
      Nothing
  ]

newtype Room = Room String deriving (Eq, Ord)

data Meeting a = Meeting {
    name :: T.Text
  , speaker :: T.Text
  , dateTime :: a
  , room :: Room
  , description :: T.Text
  , slidesUrl :: Maybe T.Text
  } deriving (Show, Eq, Functor)

meetingAt :: String -> LocalTime
meetingAt time = readTime defaultTimeLocale "%Y-%m-%d %H:%M" time

instance Default Room where
  def = Room "Meshel 337"

instance Show Room where
  show (Room r) = r

instance Ord (Meeting LocalTime) where
  compare a b = compare (dateTime a) (dateTime b)

data MeetingStatus = Happened | Upcoming deriving Eq

-- | Filter from 'upcomingEvents' only those meetings that have not yet
-- happened.
filterMeetings :: MeetingStatus -> IO [Meeting LocalTime]
filterMeetings s = do
  current <- zonedTimeToUTC <$> getZonedTime
  tz <- getCurrentTimeZone
  let meetingsUTC = (fmap (localTimeToUTC tz)) <$> upcomingEvents
      filtered = filter (filterFunc current) meetingsUTC
      filteredLocal = (fmap (utcToLocalTime tz)) <$> filtered
  return filteredLocal
  where
    calcOffset c d = fromRational . toRational $ diffUTCTime c d
    filterFunc current m =
      if s == Upcoming
      then calcOffset current (dateTime m) < 0
      else calcOffset current (dateTime m) > 0

newMeetings :: IO [Meeting LocalTime]
newMeetings = filterMeetings Upcoming

previousMeetings :: IO [Meeting LocalTime]
previousMeetings = filterMeetings Happened

nextMeeting :: IO (Meeting LocalTime)
nextMeeting = head <$> newMeetings

-- Why am I not using lenses here?
formatMeetingTime :: FormatTime a => Meeting a -> Meeting String
formatMeetingTime m = m { dateTime = fmtTime (dateTime m) }
  where
    fmtTime = formatTime defaultTimeLocale "%m/%d/%Y at %l:%M%P"
