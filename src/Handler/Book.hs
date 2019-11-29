{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Book where

import Import

getBookR :: BookEntityId -> Handler Value
getBookR bookId = do
  book <- runDB $ get404 bookId
  returnJson $ Entity bookId book
