{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Book where

import           Data.Aeson
import           Data.Aeson.TH
import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import           Import

putBookR :: BookEntityId -> Handler Value
putBookR bookId = do
  book <- requireCheckJsonBody :: Handler BookEntity
  _ <- runDB $ replace bookId book
  sendResponseStatus status200 ("UPDATED" :: String)

getBookR :: BookEntityId -> Handler Value
getBookR bookId = do
  book <- runDB $ get404 bookId
  returnJson $ Entity bookId book

deleteBookR :: BookEntityId -> Handler Value
deleteBookR bookId = do
  _ <- runDB $ delete bookId
  sendResponseStatus status200 ("DELETED" :: String)

getBookAuthorsR :: BookEntityId -> Handler Value
getBookAuthorsR bookId = do
  authors <-
    runDB $
    E.select $
    E.from $ \(author `E.InnerJoin` authorBook) -> do
      E.on $ author ^. AuthorEntityId E.==. authorBook ^. AuthorBookEntityAuthorId
      E.where_ $ authorBook ^. AuthorBookEntityBookId E.==. E.val bookId
      return author
  returnJson authors

postBookAuthorsR :: BookEntityId -> Handler ()
postBookAuthorsR bookId = do
  authorIds <- requireCheckJsonBody :: Handler [AuthorEntityId]
  let authorsBooks = map (`AuthorBookEntity` bookId) authorIds
  _ <- runDB $ forM authorsBooks insert400
  sendResponseStatus status201 ("CREATED" :: String)

deleteBookAuthorR :: BookEntityId -> AuthorEntityId -> Handler Value
deleteBookAuthorR bookId authorId = do
  _ <- runDB $ deleteWhere [AuthorBookEntityAuthorId ==. authorId, AuthorBookEntityBookId ==. bookId]
  sendResponseStatus status200 ("DELETED" :: String)

getBookChartersR :: BookEntityId -> Handler Value
getBookChartersR bookId = do
  charters <- runDB $ selectList [CharterEntityBookId ==. bookId] [] :: Handler [Entity CharterEntity]
  returnJson charters

data CharterEntityPartial =
  CharterEntityPartial
    { title :: Text
    }

deriveFromJSON defaultOptions ''CharterEntityPartial

postBookChartersR :: BookEntityId -> Handler Value
postBookChartersR bookId = do
  charterPartial <- requireCheckJsonBody :: Handler CharterEntityPartial
  _ <- runDB $ insert $ CharterEntity (title charterPartial) bookId
  sendResponseStatus status201 ("CREATED" :: String)

deleteBookCharterR :: BookEntityId -> CharterEntityId -> Handler Value
deleteBookCharterR bookId charterId = do
  _ <- runDB $ deleteWhere [CharterEntityId ==. charterId, CharterEntityBookId ==. bookId]
  sendResponseStatus status200 ("DELETED" :: String)

putBookCharterR :: BookEntityId -> CharterEntityId -> Handler Value
putBookCharterR bookId charterId = do
  charterPartial <- requireCheckJsonBody :: Handler CharterEntityPartial
  _ <- runDB $ replace charterId $ CharterEntity (title charterPartial) bookId
  sendResponseStatus status200 ("UPDATED" :: String)

getBookHistoryR :: BookEntityId -> Handler Value
getBookHistoryR bookId = do
  history <- runDB $ selectList [HistoryEntityBookId ==. bookId] [] :: Handler [Entity HistoryEntity]
  returnJson history

data HistoryBookPartial =
  HistoryBookPartial
    { returnedDate     :: Maybe Day
    , realReturnedDate :: Maybe Day
    , takenDate        :: Day
    , status           :: Text
    , readerId         :: ReaderEntityId
    }

deriveFromJSON defaultOptions ''HistoryBookPartial

postBookHistoryR :: BookEntityId -> Handler Value
postBookHistoryR bookId = do
  historyPartial <- requireCheckJsonBody :: Handler HistoryBookPartial
  _ <- runDB $
    insert $
    HistoryEntity
      (returnedDate historyPartial)
      (realReturnedDate historyPartial)
      (takenDate historyPartial)
      (status historyPartial)
      bookId
      (readerId historyPartial)
  sendResponseStatus status201 ("CREATED" :: String)

putBookHistoryItemR :: BookEntityId -> HistoryEntityId -> Handler Value
putBookHistoryItemR bookId historyId = do
  historyPartial <- requireCheckJsonBody :: Handler HistoryBookPartial
  _ <- runDB $
    replace historyId $
    HistoryEntity
      (returnedDate historyPartial)
      (realReturnedDate historyPartial)
      (takenDate historyPartial)
      (status historyPartial)
      bookId
      (readerId historyPartial)
  sendResponseStatus status200 ("UPDATED" :: String)
