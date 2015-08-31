module Handler.Mock where


import Import
import Database.Persist.Sql(toSqlKey)


getInitR :: Handler Html
getInitR = do
    now <- liftIO getCurrentTime

    _ <- runDB $ deleteWhere ([] :: [Filter Stock])
    _ <- runDB $ insert $ Stock "ノバレーゼ" 2128 "結婚式場の運営" "http://www.novarese.co.jp/corp/" "高め、28ページ"
    _ <- runDB $ insert $ Stock "元気寿司" 9828 "回転寿司店" "http://www.genkisushi.co.jp/" "良さげ、148ページ"
    _ <- runDB $ insert $ Stock "ビックカメラ" 3048 "家電 首都圏が中心" "http://www.biccamera.co.jp/bicgroup/index.html" "あはは、88ページ"
    _ <- runDB $ insert $ Stock "シダックス" 4837 "給食、レストラン、カラオケ" "http://www.shidax.co.jp/" "うはは、108ページ"

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

    _ <- runDB $ deleteWhere ([] :: [Filter Note])
    _ <- runDB $ insert $ Note "FF13概要" (Textarea "**パルス**の**ファルス**の**ルシ**が**コクーン**で**パージ**") now

    _ <- runDB $ deleteWhere ([] :: [Filter Tag])
    _ <- runDB $ insert $ Tag "口座"
    _ <- runDB $ insert $ Tag "メモ"
    _ <- runDB $ insert $ Tag "銘柄"

    redirect $ StockListR
