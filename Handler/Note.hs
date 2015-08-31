module Handler.Note where


import Import
import Data.Time


fNote :: Maybe Note -> Html -> MForm Handler (FormResult Note, Widget)
fNote mNote = renderTable $ Note
    <$> areq textField     (createSettings [("placeholder", "タイトル")])  (noteTitle <$> mNote)
    <*> areq textareaField (createSettings [("placeholder", "内容")])     (noteBody  <$> mNote)
    <*> lift (liftIO getCurrentTime)


getNoteListR :: Handler Html
getNoteListR = do
    notes <- runDB $ selectList [] [Asc NoteId]
    let contents = notes

    tz <- liftIO getCurrentTimeZone

    defaultLayout $ do
        markdownWidget

        $(widgetFile "note/list")


getNoteCreateR :: Handler Html
getNoteCreateR = do
    (formWidget, enctype) <- generateFormPost $ fNote Nothing
    let route = NoteCreateR

    defaultLayout $ do
        markdownWidget

        $(widgetFile "note/form")


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
    let route = NoteUpdateR stockId

    defaultLayout $ do
        markdownWidget

        $(widgetFile "note/form")


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
