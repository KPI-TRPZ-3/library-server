{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Common handler functions.
module Handler.Common where

import           Data.FileEmbed (embedFile)
import           Import
-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.
