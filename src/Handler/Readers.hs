{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Readers where

import           Import

getReadersR :: Handler Value
getReadersR = do
  readers <- runDB $ selectList [] [] :: Handler [Entity ReaderEntity]
  returnJson readers

postReadersR :: Handler Value
postReadersR = do
  reader <- requireCheckJsonBody :: Handler ReaderEntity
  _ <- runDB $ insert reader
  sendResponseStatus status201 "CREATED"


