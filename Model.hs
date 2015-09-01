{-# LANGUAGE FlexibleInstances   #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


data StockItem = StockItem { name     :: Text,
                             code     :: Int,
                             desc     :: Text,
                             url      :: Text,
                             note     :: Text,
                             presents :: [(PresentId, Present)],
                             timings  :: [(TimingId, Timing)]
                           }


instance ToJSON StockItem where
    toJSON (StockItem name code desc url note presents timings) = object [ "name"     .= name,
                                                                           "code"     .= code,
                                                                           "desc"     .= desc,
                                                                           "url"      .= url,
                                                                           "note"     .= note,
                                                                           "presents" .= presents,
                                                                           "timings"  .= timings
                                                                         ]
