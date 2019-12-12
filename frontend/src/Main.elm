module Main exposing (..)

-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)

import Browser
import Button exposing (..)
import Element exposing (..)
import Element.Border as Border
import Element.Font exposing (size)
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, bool, field, int, list, map4, string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success (List String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getTodos )



-- UPDATE


type Msg
    = Reload
    | GotTodos (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reload ->
            ( Loading, getTodos )

        GotTodos result ->
            case result of
                Ok todos ->
                    ( Success todos, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        column [ padding 12, spacing 12, width fill ]
            [ el [ size 24 ] <| text "Topics"
            , button [ padding 12 ]
                { onPress = Just Reload
                , label = text "Refresh"
                }
            , viewTopics model
            ]


viewTopics : Model -> Element Msg
viewTopics model =
    case model of
        Failure ->
            el [] <| text "Failed to load topics"

        Loading ->
            text "Loading..."

        Success topics ->
            column [ spacing 12, width fill ] (List.map text topics)


edges =
    { top = 0
    , right = 0
    , left = 0
    , bottom = 0
    }



-- HTTP


getTodos : Cmd Msg
getTodos =
    Http.get
        { url = "/api/list"
        , expect = Http.expectJson GotTodos todosDecoder
        }


todosDecoder : Decoder (List String)
todosDecoder =
    Json.Decode.list string
