module Handler.Mock where


import Import
import Database.Persist.Sql(toSqlKey)


getInitR :: Handler Html
getInitR = do
    _ <- runDB $ deleteWhere ([] :: [Filter Stock])
    _ <- runDB $ insert $ Stock "ノバレーゼ" 2128 "結婚式場の運営" "http://www.novarese.co.jp/corp/" "高め"
    _ <- runDB $ insert $ Stock "元気寿司" 9828 "回転寿司店" "http://www.genkisushi.co.jp/" "良さげ"

    _ <- runDB $ deleteWhere ([] :: [Filter Present])
    _ <- runDB $ insert $ Present 100 "レストラン優待券（30%割引）x 1" (toSqlKey 1 :: StockId)
    _ <- runDB $ insert $ Present 100 "ギフト商品優待券（20%割引）x 4" (toSqlKey 1 :: StockId)
    _ <- runDB $ insert $ Present 100 "優待食事券（500円）x 3" (toSqlKey 2 :: StockId)
    _ <- runDB $ insert $ Present 500 "優待食事券（500円）x 15" (toSqlKey 2 :: StockId)
    _ <- runDB $ insert $ Present 1000 "優待食事券（500円）x 30" (toSqlKey 2 :: StockId)

    _ <- runDB $ deleteWhere ([] :: [Filter Timing])
    _ <- runDB $ insert $ Timing "12月末" (toSqlKey 1 :: StockId)
    _ <- runDB $ insert $ Timing "3月末" (toSqlKey 2 :: StockId)
    _ <- runDB $ insert $ Timing "9月末" (toSqlKey 2 :: StockId)

    redirect $ StockListR
