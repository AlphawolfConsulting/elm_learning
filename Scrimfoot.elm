module Scrimfoot exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Array exposing (Array)
import Random 
import Http exposing (..)
import Html.Attributes as Attr exposing ( id, class, classList, src, name, max, type_, title )
import Json.Decode exposing (string, int, list, Decoder, at, Value)
import Json.Decode.Pipeline exposing (decode, required, optional)

type alias Model =
    { title: String
    , copyright: String
    , tos: Html Msg
    , content : Html Msg
    }

type PageType
    = ContactUs Int
    | AboutUs Int
    | Services Int
    | Home Int
    | TOS Int

type Msg 
    = SetTitle String
    | SetCopyright String
    | SelectHome
    | SelectServices
    | SelectAboutUs
    | SelectContactUs
    | SelectTOS
    
update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        SetTitle title ->
            ({model | title = title}, Cmd.none)

        SetCopyright copyright ->
            ({model | copyright = copyright}, Cmd.none)

        SelectHome ->
            ({model | content = getPageContent (Home 1) }, Cmd.none)
        SelectAboutUs ->
            ({model | content = getPageContent (AboutUs 1) }, Cmd.none)

        SelectContactUs ->
            ({model | content = getPageContent (ContactUs 1) }, Cmd.none)

        SelectServices ->
            ({model | content = getPageContent (Services 1) }, Cmd.none)

        SelectTOS ->
            ({model | content = getPageContent (TOS 1) }, Cmd.none)

getTOS : Html Msg
getTOS =
    div [ id "main-tos"]
    [ text  
    """
    Your use is governed by this PDF -> the PDF
    """
    ]
getMainPage : Html Msg
getMainPage =
    div [ id "main-main"]
    [ text
    """
    Here is the main page content
    """
    ]

getPageContent : PageType -> Html Msg
getPageContent pt =
    let
        getContent : Html Msg
        getContent =
            case pt of
                Home ver ->
                    span [] [ text "Home Page Content" ]

                AboutUs ver ->
                    span [] [ text "About Us Page Content" ]

                ContactUs ver ->
                    span [] [ text "Contact Us Page Content" ]

                Services ver ->
                    span [] [ text "Services Page Content" ]

                TOS ver ->
                    span [] [ text "Terms of Service Page Content" ]
    in
        getContent 


initialModel : Model
initialModel = 
    {
        title = """Scrimfoot Management LLC"""
        , copyright = """Copyright 2004-2018, all rights reserved"""
        , tos = getPageContent (TOS 1)
        , content = getPageContent (Home 1)
    }

viewMainMenu : PageType -> Html Msg
viewMainMenu pt =
    div [ class "main-nav"] (List.map makeNavItem ["Home","Services","About Us"])


makeNavItem : String -> Html Msg
makeNavItem item =
    a [ class "nav-item"] 
    [ span [] [text item]]

view : Model -> Html Msg
view model =
    div [ class "content" ]
    [ h1 [] [ text "Scrimfoot Management LLC"]
    , viewMainMenu (Home 1)
    , div [ id "content-main"] [ model.content ]
    , div [ class "footer" ] 
        [ span [class "copyright"] [ text model.copyright ] ]
    ]

main : Program Never Model Msg
main =
        Html.program
            { init = (initialModel, Cmd.none)
            , view = view
            , update = update
            , subscriptions = subscriptions
        }
    
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
