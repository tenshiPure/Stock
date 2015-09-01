module Handler.Stock where


import Import


fStock :: Maybe Stock -> Html -> MForm Handler (FormResult Stock, Widget)
fStock mStock = renderDivs $ Stock
    <$> areq textField (createSettings [("placeholder", "銘柄名")])     (stockName <$> mStock)
    <*> areq intField  (createSettings [("placeholder", "証券コード")]) (stockCode <$> mStock)
    <*> areq textField (createSettings [("placeholder", "会社概要")])   (stockDesc <$> mStock)
    <*> areq urlField  (createSettings [("placeholder", "URL")])        (stockUrl  <$> mStock)
    <*> areq textField (createSettings [("placeholder", "コメント")])   (stockNote <$> mStock)


getStocksR :: Handler Value
getStocksR = do
    stocks <- runDB $ selectList [] [Asc StockId]
    contents <- forM stocks $ \eStock -> do
        let stockId = entityKey eStock
        let stock =   entityVal eStock

        ePresents <- runDB $ selectList [PresentStockId ==. stockId] [Asc PresentCount]
        let presentIds = map entityKey ePresents
        let presents   = map entityVal ePresents

        eTimings <- runDB $ selectList [TimingStockId ==. stockId] [Asc TimingDate]
        let timingIds = map entityKey eTimings
        let timings   = map entityVal eTimings

        return $ StockItem (stockName stock) (stockCode stock) (stockDesc stock) (stockUrl stock) (stockNote stock) (zip presentIds presents) (zip timingIds timings)

    returnJson contents


getStockListR :: Handler Html
getStockListR = do
    defaultLayout $(widgetFile "stock/list")


getStockCreateR :: Handler Html
getStockCreateR = do
    (formWidget, enctype) <- generateFormPost $ fStock Nothing

    defaultLayout $(widgetFile "stock/create")


postStockCreateR :: Handler Html
postStockCreateR = do
    ((res, _), _) <- runFormPost $ fStock Nothing
    case res of
        FormSuccess stock -> do
            _ <- runDB $ insert stock
            redirect $ StockListR

        _ -> redirect $ StockListR


getStockUpdateR :: StockId -> Handler Html
getStockUpdateR stockId = do
    stock <- runDB $ get404 stockId
    (formWidget, enctype) <- generateFormPost $ fStock (Just stock)

    defaultLayout $(widgetFile "stock/update")


postStockUpdateR :: StockId -> Handler Html
postStockUpdateR stockId = do
    ((res, _), _) <- runFormPost $ fStock Nothing

    case res of
        FormSuccess stock -> do
            _ <- runDB $ replace stockId stock
            redirect $ StockListR

        _ -> redirect $ StockListR


getStockDeleteR :: StockId -> Handler Html
getStockDeleteR stockId = do
    _ <- runDB $ delete stockId
    redirect $ StockListR
