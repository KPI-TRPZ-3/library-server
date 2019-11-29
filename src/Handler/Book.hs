{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Book where

import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import           Import

putBookR :: BookEntityId -> Handler Value
putBookR bookId = do
  book <- requireCheckJsonBody :: Handler BookEntity
  runDB $ replace bookId book
  sendResponseStatus status200 "UPDATED"

getBookR :: BookEntityId -> Handler Value
getBookR bookId = do
  book <- runDB $ get404 bookId
  returnJson $ Entity bookId book

deleteBookR :: BookEntityId -> Handler Value
deleteBookR bookId = do
  runDB $ delete bookId
  sendResponseStatus status200 "DELETED"

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
  _ <- runDB $ forM authorsBooks insert
  sendResponseStatus status201 "CREATED"

deleteBookAuthorR :: BookEntityId -> AuthorEntityId -> Handler Value
deleteBookAuthorR bookId authorId = do
  runDB $ deleteWhere [AuthorBookEntityAuthorId ==. authorId, AuthorBookEntityBookId ==. bookId]
  sendResponseStatus status200 "DELETED"
