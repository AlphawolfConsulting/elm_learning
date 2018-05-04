module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)
import Random 

type alias Photo =
    { url : String }

type alias Model = 
    {  photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }

type Msg 
    = SelectedByUrl String
    | SurpriseMe
    | SetSize ThumbnailSize
    | SeletectByIndex Int

type ThumbnailSize
    = Small
    | Medium
    | Large

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        SelectedByUrl url ->
            ({ model | selectedUrl = url },Cmd.none)
        SurpriseMe ->
            ( model, Random.generate SeletectByIndex randomPhotoPicker )
        SetSize size ->
            ( { model | chosenSize = size}, Cmd.none)
        SeletectByIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )

initialModel : Model
initialModel = 
    {
        photos = 
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg"}
        ]
        , selectedUrl = "1.jpeg"
        , chosenSize = Small
    }

photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos

urlPrefix : String
urlPrefix = 
    "http://elm-in-action.com/"

getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url
        Nothing ->
            ""

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

randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 ( Array.length photoArray - 1 )

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img [ src (urlPrefix ++ thumbnail.url)
            , classList [ ( "selected", selectedUrl == thumbnail.url ) ]
            , onClick ( SelectedByUrl thumbnail.url )
            ]
            []

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick ( SetSize size ) ] []
        , text ( sizeToString size )
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
        , img 
            [ class "large" 
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]
main : Program Never Model Msg
main = 
    Html.program
    {   init = ( initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
    }
