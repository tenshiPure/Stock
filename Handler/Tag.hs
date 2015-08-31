module Handler.Tag where


import Import


fTag :: Maybe Tag -> Html -> MForm Handler (FormResult Tag, Widget)
fTag mTag = renderDivs $ Tag
    <$> areq textField   (createSettings [("placeholder", "ラベル")]) (tagLabel <$> mTag)


getTagListR :: Handler Html
getTagListR = do
    tags <- runDB $ selectList [] [Asc TagId]
    let contents = tags

    defaultLayout $(widgetFile "tag/list")


getTagCreateR :: Handler Html
getTagCreateR = do
    (formWidget, enctype) <- generateFormPost $ fTag Nothing

    defaultLayout $(widgetFile "tag/create")


postTagCreateR :: Handler Html
postTagCreateR = do
    ((res, _), _) <- runFormPost $ fTag Nothing
    case res of
        FormSuccess tag -> do
            _ <- runDB $ insert tag
            redirect $ TagListR

        _ -> redirect $ TagListR


getTagUpdateR :: TagId -> Handler Html
getTagUpdateR tagId = do
    _tag <- runDB $ get404 tagId
    (formWidget, enctype) <- generateFormPost $ fTag (Just _tag)

    defaultLayout $(widgetFile "tag/update")


postTagUpdateR :: TagId -> Handler Html
postTagUpdateR tagId = do
    _tag <- runDB $ get404 tagId
    ((res, _), _) <- runFormPost $ fTag Nothing

    case res of
        FormSuccess tag -> do
            _ <- runDB $ replace tagId tag
            redirect $ TagListR

        _ -> redirect $ TagListR


getTagDeleteR :: TagId -> Handler Html
getTagDeleteR tagId = do
    _ <- runDB $ delete tagId
    redirect $ TagListR
