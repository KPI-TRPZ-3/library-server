{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Authors where

import           Import

getAuthorsR :: Handler Value
getAuthorsR = do
  authors <- runDB $ selectList [] [] :: Handler [Entity AuthorEntity]
  returnJson authors

postAuthorsR :: Handler Value
postAuthorsR = do
  author <- requireCheckJsonBody :: Handler AuthorEntity
  _ <- runDB $ insert author
  sendResponseStatus status201 "CREATED"
