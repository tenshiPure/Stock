module Handler.Note where


import Import
import Data.Time


fNote :: Maybe Note -> Html -> MForm Handler (FormResult Note, Widget)
fNote mNote = renderTable $ Note
    <$> areq textField     (createSettings [("placeholder", "タイトル")])  (noteTitle <$> mNote)
    <*> areq textareaField (createSettings [("placeholder", "内容")])     (noteBody  <$> mNote)
    <*> lift (liftIO getCurrentTime)


getTags :: HandlerT App IO [((Entity Tag), Bool)]
getTags = do
    _tags <- runDB $ selectList [] [Asc TagId]
    tags <- forM _tags $ \tag -> do
        return (tag, False)

    return tags


getTagsByNoteId :: NoteId -> HandlerT App IO [((Entity Tag), Bool)]
getTagsByNoteId noteId = do
    _tags <- runDB $ selectList [] [Asc TagId]
    tags <- forM _tags $ \tag -> do
        mTagging <- runDB $ selectFirst [TaggingNoteId ==. noteId, TaggingTagId ==. entityKey tag] [Asc TaggingId]
        return $ case mTagging of
            Just _ -> (tag, True)
            Nothing -> (tag, False)

    return tags


getNoteListR :: Handler Html
getNoteListR = do
    notes <- runDB $ selectList [] [Asc NoteId]
    contents <- forM notes $ \note -> do
        taggings <- runDB $ selectList [TaggingNoteId ==. entityKey note] [Asc TaggingId]
        let tagIds = map (taggingTagId . entityVal) taggings
        tags <- runDB $ selectList [TagId <-. tagIds] [Asc TagId]

        return (note, tags)

    tz <- liftIO getCurrentTimeZone

    defaultLayout $ do
        markdownWidget

        $(widgetFile "note/list")


getNoteCreateR :: Handler Html
getNoteCreateR = do
    (formWidget, enctype) <- generateFormPost $ fNote Nothing
    let route = NoteCreateR
    tags <- getTags

    defaultLayout $ do
        markdownWidget

        $(widgetFile "note/form")


-- todo
postNoteCreateR :: Handler Html
postNoteCreateR = do
    ((res, _), _) <- runFormPost $ fNote Nothing
    case res of
        FormSuccess note -> do
            _ <- runDB $ insert note
            redirect $ NoteListR

        _ -> redirect $ NoteListR


getNoteUpdateR :: NoteId -> Handler Html
getNoteUpdateR noteId = do
    note <- runDB $ get404 noteId
    (formWidget, enctype) <- generateFormPost $ fNote (Just note)
    let route = NoteUpdateR noteId
    tags <- getTagsByNoteId noteId

    defaultLayout $ do
        markdownWidget

        $(widgetFile "note/form")


postNoteUpdateR :: NoteId -> Handler Html
postNoteUpdateR noteId = do
    ((res, _), _) <- runFormPost $ fNote Nothing

    case res of
        FormSuccess note -> do
            _ <- runDB $ replace noteId note
            redirect $ NoteListR

        _ -> redirect $ NoteListR


getNoteDeleteR :: NoteId -> Handler Html
getNoteDeleteR noteId = do
    _ <- runDB $ delete noteId
    redirect $ NoteListR
