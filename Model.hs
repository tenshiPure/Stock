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


data StockItem = StockItem { stock      :: (Entity Stock),
                             presents   :: [(Entity Present)],
                             timings    :: [(Entity Timing)]
                           }


instance ToJSON StockItem where
    toJSON (StockItem stock presents timings) = object [ "stock"    .= stock,
                                                         "presents" .= presents,
                                                         "timings"  .= timings
                                                       ]


instance ToJSON (Entity Stock) where
    toJSON (Entity stockId (Stock name code desc url note)) = object [ "id"   .= stockId,
                                                                       "name" .= name,
                                                                       "code" .= code,
                                                                       "desc" .= desc,
                                                                       "url"  .= url,
                                                                       "note" .= note
                                                                     ]


instance ToJSON (Entity Present) where
    toJSON (Entity presentId (Present _count desc _)) = object [ "id"   .= presentId,
                                                                "count" .= _count,
                                                                "desc"  .= desc
                                                              ]


instance ToJSON (Entity Timing) where
    toJSON (Entity timingId (Timing date _)) = object [ "id"   .= timingId,
                                                        "date" .= date
                                                      ]
