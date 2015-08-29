module Handler.Present where


import Import


fPresent :: Maybe Present -> StockId -> Html -> MForm Handler (FormResult Present, Widget)
fPresent mPresent stockId = renderDivs $ Present
    <$> areq intField "" (presentCount <$> mPresent)
    <*> areq textField "" (presentDesc <$> mPresent)
    <*> areq hiddenField "" (Just stockId)


getPresentCreateR :: StockId -> Handler Html
getPresentCreateR stockId = do
    (formWidget, enctype) <- generateFormPost $ fPresent Nothing stockId

    defaultLayout $(widgetFile "present/create")


postPresentCreateR :: StockId -> Handler Html
postPresentCreateR stockId = do
    ((res, _), _) <- runFormPost $ fPresent Nothing stockId
    case res of
        FormSuccess present -> do
            _ <- runDB $ insert present
            redirect $ StockListR

        _ -> redirect $ StockListR


getPresentUpdateR :: PresentId -> Handler Html
getPresentUpdateR presentId = do
    _present <- runDB $ get404 presentId
    (formWidget, enctype) <- generateFormPost $ fPresent (Just _present) (presentStockId _present)

    defaultLayout $(widgetFile "present/update")


postPresentUpdateR :: PresentId -> Handler Html
postPresentUpdateR presentId = do
    _present <- runDB $ get404 presentId
    ((res, _), _) <- runFormPost $ fPresent Nothing (presentStockId _present)

    case res of
        FormSuccess present -> do
            _ <- runDB $ replace presentId present
            redirect $ StockListR

        _ -> redirect $ StockListR


getPresentDeleteR :: PresentId -> Handler Html
getPresentDeleteR presentId = do
    _ <- runDB $ delete presentId
    redirect $ StockListR
