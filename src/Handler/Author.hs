{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Author where

import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import           Import

getAuthorR :: AuthorEntityId -> Handler Value
getAuthorR authorId = do
  author <- runDB $ get404 authorId
  returnJson $ Entity authorId author

deleteAuthorR :: AuthorEntityId -> Handler Value
deleteAuthorR authorId = do
  _ <- runDB $ delete authorId
  sendResponseStatus status200 ("DELETED" :: String)

putAuthorR :: AuthorEntityId -> Handler Value
putAuthorR authorId = do
  author <- requireCheckJsonBody :: Handler AuthorEntity
  _ <- runDB $ replace authorId author
  sendResponseStatus status200 ("UPDATED" :: String)

getAuthorBooksR :: AuthorEntityId -> Handler Value
getAuthorBooksR authorId = do
  books <-
    runDB $
    E.select $
    E.from $ \(book `E.InnerJoin` authorBook) -> do
      E.on $ book ^. BookEntityId E.==. authorBook ^. AuthorBookEntityBookId
      E.where_ $ authorBook ^. AuthorBookEntityAuthorId E.==. E.val authorId
      return book
  returnJson books

postAuthorBooksR :: AuthorEntityId -> Handler Value
postAuthorBooksR authorId = do
  bookIds <- requireCheckJsonBody :: Handler [BookEntityId]
  let authorsBooks = map (AuthorBookEntity authorId) bookIds
  _ <- runDB $ forM authorsBooks insert400
  sendResponseStatus status201 ("CREATED" :: String)

deleteAuthorBookR :: AuthorEntityId -> BookEntityId -> Handler Value
deleteAuthorBookR authorId bookId = do
  _ <- runDB $ deleteWhere [AuthorBookEntityAuthorId ==. authorId, AuthorBookEntityBookId ==. bookId]
  sendResponseStatus status200 ("DELETED" :: String)
