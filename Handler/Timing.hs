module Handler.Timing where


import Import


fTiming :: Maybe Timing -> StockId -> Html -> MForm Handler (FormResult Timing, Widget)
fTiming mTiming stockId = renderDivs $ Timing
    <$> areq textField   (createSettings [("placeholder", "契約確定日")]) (timingDate <$> mTiming)
    <*> areq hiddenField ""                                           (Just stockId)


getTimingCreateR :: StockId -> Handler Html
getTimingCreateR stockId = do
    (formWidget, enctype) <- generateFormPost $ fTiming Nothing stockId

    defaultLayout $(widgetFile "timing/create")


postTimingCreateR :: StockId -> Handler Html
postTimingCreateR stockId = do
    ((res, _), _) <- runFormPost $ fTiming Nothing stockId
    case res of
        FormSuccess timing -> do
            _ <- runDB $ insert timing
            redirect $ StockListR

        _ -> redirect $ StockListR


getTimingUpdateR :: TimingId -> Handler Html
getTimingUpdateR timingId = do
    _timing <- runDB $ get404 timingId
    (formWidget, enctype) <- generateFormPost $ fTiming (Just _timing) (timingStockId _timing)

    defaultLayout $(widgetFile "timing/update")


postTimingUpdateR :: TimingId -> Handler Html
postTimingUpdateR timingId = do
    _timing <- runDB $ get404 timingId
    ((res, _), _) <- runFormPost $ fTiming Nothing (timingStockId _timing)

    case res of
        FormSuccess timing -> do
            _ <- runDB $ replace timingId timing
            redirect $ StockListR

        _ -> redirect $ StockListR


getTimingDeleteR :: TimingId -> Handler Html
getTimingDeleteR timingId = do
    _ <- runDB $ delete timingId
    redirect $ StockListR
