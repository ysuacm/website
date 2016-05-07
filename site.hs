{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Monoid ((<>), mconcat)
import qualified Data.Text as T
import Data.Thyme
import Hakyll
import Text.Pandoc.Options (readerSmart)

import Meetings

createMeetingsListPage
  :: FormatTime a
  => MeetingStatus
  -> Identifier
  -> String
  -> [Meeting a]
  -> [Meeting a]
  -> Rules ()
createMeetingsListPage s fn title previous upcoming =
  create [fn] $ do
    route idRoute
    compile $ do
      let meetings = reverse (formatMeetingTime <$> if s == Happened then previous else upcoming)
          archiveCtx =
            listField "presentations" presentationCtx (sequence <$> makeItem meetings) <>
            constField "title" title <>
            constField "id" (if s == Happened then "previous" else "upcoming") <>
            constField "highlight" "true" <>
            defaultContext
          filename =
            if s == Happened
            then "templates/archive.html"
            else "templates/events.html"

      makeItem ""
        >>= loadAndApplyTemplate filename archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

main :: IO ()
main = do
  next <- formatMeetingTime <$> nextMeeting
  previous <- previousMeetings
  upcoming <- newMeetings

  hakyll $ do
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "static/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    createMeetingsListPage Upcoming "events.html" "Upcoming Meetings" previous upcoming
    createMeetingsListPage Happened "presentations.html" "Presentation Archive" previous upcoming

    match "*.html" $ do
      route $ setExtension "html"
      compile $ do
        let meetings = reverse (formatMeetingTime <$> previous)
            indexCtx = mconcat
                       [ constField "title" "Home"
                       , constField "nm_name" (T.unpack $ name next)
                       , constField "nm_speaker" (T.unpack $ speaker next)
                       , constField "nm_dt" (dateTime next)
                       , constField "nm_room" (show $ room next)
                       , constField "nm_desc" (T.unpack $ description next)
                       , listField "presentations" presentationCtx (sequence <$> makeItem meetings)
                       , defaultContext
                       ]

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

presentationCtx :: Context (Meeting String)
presentationCtx = mconcat
  [ field "nm_name" $ return . T.unpack . name . itemBody
  , field "nm_speaker" $ return . T.unpack . speaker . itemBody
  , field "nm_dt" $ return . dateTime . itemBody
  , field "nm_room" $ return . show . room . itemBody
  , field "nm_desc" $ return . T.unpack . description . itemBody
  ]
