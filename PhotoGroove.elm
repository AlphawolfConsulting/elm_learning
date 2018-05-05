module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random 
import Http exposing (..)
import Html.Attributes exposing ( id, class, classList, src, name, type_, title )
import Json.Decode exposing (string, int, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)

type alias Photo =
    { url : String
    , size : Int
    , title : String
    }

type ThumbnailSize
    = Small
    | Medium
    | Large

type alias Model = 
    {  photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    }

type Msg 
    = SelectedByUrl String
    | SurpriseMe
    | SetSize ThumbnailSize
    | SeletectByIndex Int
    | LoadPhotos ( Result Http.Error (List Photo) )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        SelectedByUrl url ->
            ({ model | selectedUrl = Just url },Cmd.none)

        SurpriseMe ->
            let
                randomPhotoPicker = Random.int 0 ( List.length model.photos - 1 )
            in
                ( model, Random.generate SeletectByIndex randomPhotoPicker )

        SetSize size ->
            ( { model | chosenSize = size}, Cmd.none)

        SeletectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                    |> Array.fromList
                    |> Array.get index
                    |> Maybe.map .url
            in
                ( { model | selectedUrl = newSelectedUrl }, Cmd.none )

        LoadPhotos (Ok photos) ->
            ( { model | photos = photos, selectedUrl = Maybe.map .url (List.head photos)}, Cmd.none )

        LoadPhotos (Err _ ) ->
            ( { model | loadingError = Just "We got an error loading images list. You might try turning your machine off and on?"}, Cmd.none)

photoDecoder : Decoder Photo
photoDecoder =
    decode buildPhoto
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"

buildPhoto : String -> Int -> String -> Photo
buildPhoto url size title =
    { url = url, size = size, title = title }

initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send LoadPhotos

initialModel : Model
initialModel = 
    {
        photos = [ ]
        , selectedUrl = Nothing
        , loadingError = Nothing
        , chosenSize = Small
    }

photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos

urlPrefix : String
urlPrefix = 
    "http://elm-in-action.com/"

getPhotoUrl : Int -> Maybe String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            Just photo.url
        Nothing ->
            Nothing

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"
        Medium ->
            "medium"
        Large ->
            "large"

sizeToClass : ThumbnailSize -> String
sizeToClass size =
    case size of
        Small ->
            "small"
        Medium ->
            "med"
        Large ->
            "large"

viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img [ src (urlPrefix ++ thumbnail.url)
            , title ( thumbnail.title ++ " [ " ++ toString thumbnail.size ++ " KB ]")
            , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
            , onClick ( SelectedByUrl thumbnail.url )
            ]
            []

viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl =
    case maybeUrl of
        Nothing ->
            text ""
        Just url ->
            img [ class "large", src (urlPrefix ++ "large/" ++ url) ] []

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick ( SetSize size ) ] []
        , text ( sizeToString size )
        ]

viewHttpError: Model -> Html Msg
viewHttpError model =
    case model.loadingError of
        Nothing ->
            view model
        Just errorMessage ->
            div [ class "error-message"] 
            [ h1 [] [ text "Photo Groove"]
            , p [] [ text errorMessage ]
            ]

view : Model -> Html Msg
view model = 
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button 
            [ onClick SurpriseMe ]
            [ text "Surprise Me" ]
        , h3 [] [ text "Thumbnail Size: " ]
        , div [ id "choose-size" ]
            [ viewSizeChooser Small, viewSizeChooser Medium, viewSizeChooser Large ]
        , div [ id "thumbnails", class (sizeToClass model.chosenSize) ] 
            (List.map ( viewThumbnail model.selectedUrl ) model.photos)
        , viewLarge model.selectedUrl
        ]
main : Program Never Model Msg
main = 
    Html.program
    {   init = ( initialModel, initialCmd)
        , view = viewHttpError
        , update = update
        , subscriptions = (\model -> Sub.none)
    }
