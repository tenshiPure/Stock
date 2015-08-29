module Handler.Stock where


import Import


fStock :: Maybe Stock -> Html -> MForm Handler (FormResult Stock, Widget)
fStock mStock = renderDivs $ Stock
    <$> areq textField "" (stockName <$> mStock)
    <*> areq intField "" (stockCode <$> mStock)
    <*> areq textField "" (stockDesc <$> mStock)
    <*> areq urlField "" (stockUrl <$> mStock)
    <*> areq textField "" (stockNote <$> mStock)


getStockListR :: Handler Html
getStockListR = do
    contents <- runDB $ selectList [] [Asc StockId]

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
