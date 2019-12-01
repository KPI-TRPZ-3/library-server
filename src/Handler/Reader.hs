{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Reader where

import           Import

getReaderR :: ReaderEntityId -> Handler Value
getReaderR readerId = do
  reader <- runDB $ get404 readerId
  returnJson reader

deleteReaderR :: ReaderEntityId -> Handler Value
deleteReaderR readerId = do
  _ <- runDB $ delete readerId
  sendResponseStatus status200 ("DELETED" :: String)

putReaderR :: ReaderEntityId -> Handler Value
putReaderR readerId = do
  reader <- requireCheckJsonBody :: Handler ReaderEntity
  _ <- runDB $ replace readerId reader
  sendResponseStatus status200 ("REPLACED" :: String)

getReaderBooksR :: ReaderEntityId -> Handler Value
getReaderBooksR readerId = do
  books <- runDB $ selectList [BookEntityReaderId ==. Just readerId] [] :: Handler [Entity BookEntity]
  returnJson books

postReaderBooksR :: ReaderEntityId -> Handler Value
postReaderBooksR readerId = do
  bookIds <- requireCheckJsonBody :: Handler [BookEntityId]
  _ <- runDB $ forM bookIds (\x -> update x [BookEntityReaderId =. Just readerId])
  sendResponseStatus status200 ("UPDATED" :: String)

deleteReaderBookR :: ReaderEntityId -> BookEntityId -> Handler Value
deleteReaderBookR readerId bookId = do
  _ <-
    runDB $ updateWhere [BookEntityReaderId ==. Just readerId, BookEntityId ==. bookId] [BookEntityReaderId =. Nothing]
  sendResponseStatus status200 ("DELETED" :: String)
