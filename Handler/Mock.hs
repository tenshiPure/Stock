module Handler.Mock where


import Import
-- import Database.Persist.Sql(toSqlKey, fromSqlKey)


getInitR :: Handler Html
getInitR = do
    _ <- runDB $ deleteWhere ([] :: [Filter Stock])
    _ <- runDB $ insert $ Stock "ノバレーゼ" 2128 "結婚式場の運営" "http://www.novarese.co.jp/corp/" "高め"
    _ <- runDB $ insert $ Stock "元気寿司" 9828 "回転寿司店" "http://www.genkisushi.co.jp/" "良さげ"

    redirect $ StockListR
