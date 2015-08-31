module Handler.Note where


import Import


fNote :: Maybe Note -> Html -> MForm Handler (FormResult Note, Widget)
fNote mNote = renderDivs $ Note
    <$> areq textField     (createSettings [("placeholder", "タイトル")])  (noteTitle <$> mNote)
    <*> areq textareaField (createSettings [("placeholder", "内容")])     (noteBody  <$> mNote)
    <*> lift (liftIO getCurrentTime)


getNoteListR :: Handler Html
getNoteListR = do
    notes <- runDB $ selectList [] [Asc NoteId]
    let contents = notes

    defaultLayout $ do
        addScript $ StaticR js_highlight_js
        addScript $ StaticR js_marked_js
        addStylesheet $ StaticR css_github_css

        $(widgetFile "note/list")


getNoteCreateR :: Handler Html
getNoteCreateR = do
    (formWidget, enctype) <- generateFormPost $ fNote Nothing

    defaultLayout $(widgetFile "note/create")


postNoteCreateR :: Handler Html
postNoteCreateR = do
    ((res, _), _) <- runFormPost $ fNote Nothing
    case res of
        FormSuccess note -> do
            _ <- runDB $ insert note
            redirect $ NoteListR

        _ -> redirect $ NoteListR


getNoteUpdateR :: NoteId -> Handler Html
getNoteUpdateR stockId = do
    note <- runDB $ get404 stockId
    (formWidget, enctype) <- generateFormPost $ fNote (Just note)

    defaultLayout $(widgetFile "note/update")


postNoteUpdateR :: NoteId -> Handler Html
postNoteUpdateR stockId = do
    ((res, _), _) <- runFormPost $ fNote Nothing

    case res of
        FormSuccess note -> do
            _ <- runDB $ replace stockId note
            redirect $ NoteListR

        _ -> redirect $ NoteListR


getNoteDeleteR :: NoteId -> Handler Html
getNoteDeleteR stockId = do
    _ <- runDB $ delete stockId
    redirect $ NoteListR
