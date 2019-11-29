{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Books where

import           Import

getBooksR :: Handler Value
getBooksR = do
  books <- runDB $ selectList [] [] :: Handler [Entity BookEntity]
  returnJson books

postBooksR :: Handler ()
postBooksR = do
  book <- requireCheckJsonBody :: Handler BookEntity
  _ <- runDB $ insert book
  sendResponseStatus status201 "CREATED"
