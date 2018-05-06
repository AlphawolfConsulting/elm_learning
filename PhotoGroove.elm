port module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Array exposing (Array)
import Random 
import Http exposing (..)
import Html.Attributes as Attr exposing ( id, class, classList, src, name, max, type_, title )
import Json.Decode exposing (string, int, list, Decoder, at)
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
    , status : String
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }

port setFilters : FilterOptions -> Cmd msg

type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount: Float}
    }

type Msg 
    = SelectedByUrl String
    | SeletectByIndex Int
    | SetStatus String
    | SetSize ThumbnailSize
    | SetHue Int
    | SetRipple Int
    | SetNoise Int
    | SurpriseMe
    | LoadPhotos ( Result Http.Error (List Photo) )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        SelectedByUrl url ->
            applyFilters { model | selectedUrl = Just url }

        SeletectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                    |> Array.fromList
                    |> Array.get index
                    |> Maybe.map .url
            in
                applyFilters { model | selectedUrl = newSelectedUrl }

        SetStatus status ->
            ( {model | status = status}, Cmd.none)

        SurpriseMe ->
            let
                randomPhotoPicker = Random.int 0 ( List.length model.photos - 1 )
            in
                ( model, Random.generate SeletectByIndex randomPhotoPicker )

        SetSize size ->
            ( { model | chosenSize = size}, Cmd.none)

        LoadPhotos (Ok photos) ->
            applyFilters 
                { model 
                    | photos = photos
                    , selectedUrl = Maybe.map .url (List.head photos)
                }

        LoadPhotos (Err _ ) ->
            ( { model | loadingError = Just "We got an error loading images list. You might try turning your machine off and on?"}, Cmd.none)

        SetHue hue ->
            applyFilters { model | hue = hue }

        SetRipple ripple ->
            applyFilters { model | ripple = ripple }

        SetNoise noise ->
            applyFilters { model | noise = noise }

applyFilters : Model -> ( Model , Cmd Msg )
applyFilters model =
    case model.selectedUrl of
        Just selectedUrl ->
            let
                filters = 
                [{ name = "Hue", amount = toFloat model.hue / 11}
                , {name = "Ripple", amount = toFloat model.ripple / 11}
                , {name = "Noise", amount = toFloat model.noise / 11}
                ]

                url = urlPrefix ++ "large/" ++ selectedUrl
            in
                ( model, setFilters { url = url, filters = filters})

        Nothing ->
            (model, Cmd.none)

onImmediateValueChange : ( Int ->msg) -> Attribute msg
onImmediateValueChange toMsg =
    at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed" 

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
        , status = ""
        , selectedUrl = Nothing
        , loadingError = Nothing
        , chosenSize = Small
        , hue = 0
        , ripple = 0
        , noise = 0
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

paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
    node "paper-slider"

viewPaperSlider : String -> (Int -> Msg) -> Int -> Html Msg
viewPaperSlider name toMsg magnitude = 
    div [ class "filter-slider"]
        [ label [] [ text name ]
        , paperSlider [ Attr.max "11", onImmediateValueChange toMsg
        ] []
        , label [] [ text (toString magnitude) ]
        ]
        
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
            canvas [ id "main-canvas",class "large" ] []

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
        , div [ class "status" ] [ text model.status ]
        , div [ class "filters" ]
            [ viewPaperSlider "Hue" SetHue model.hue
            , viewPaperSlider "Ripple" SetRipple model.ripple
            , viewPaperSlider "Noise" SetNoise model.noise
            ]
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
