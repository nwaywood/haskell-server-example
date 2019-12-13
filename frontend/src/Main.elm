module Main exposing (..)

import Browser
import Button exposing (..)
import Element exposing (..)
import Element.Border as Border
import Element.Font exposing (size)
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, field, int, list, map4, string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { topics : Status (List String)
    , currentTopic : SelectedTopic String
    , comments : CommentStatus (List Comment)
    }


type alias Comment =
    { id : Int
    , topic : String
    , text : String
    , time : String
    }


type SelectedTopic t
    = NoTopic
    | Topic t


type CommentStatus s
    = LoadingC
    | NotLoadedC
    | FailureC
    | SuccessC s


type Status a
    = Loading
    | Failure
    | Success a


init : () -> ( Model, Cmd Msg )
init _ =
    ( { topics = Loading, comments = NotLoadedC, currentTopic = NoTopic }, getTopics )



-- UPDATE


type Msg
    = ReloadTopics
    | SelectTopic String
    | GotTopics (Result Http.Error (List String))
    | GotComments (Result Http.Error (List Comment))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReloadTopics ->
            ( { model | topics = Loading }, getTopics )

        SelectTopic t ->
            case model.currentTopic of
                Topic oldTopic ->
                    if t == oldTopic then
                        ( model, Cmd.none )

                    else
                        ( { model | currentTopic = Topic t }, getComments t )

                NoTopic ->
                    ( { model | currentTopic = Topic t }, getComments t )

        GotTopics result ->
            case result of
                Ok ts ->
                    ( { model | topics = Success ts }, Cmd.none )

                Err _ ->
                    ( { model | topics = Failure }, Cmd.none )

        GotComments result ->
            case result of
                Ok cs ->
                    ( { model | comments = SuccessC cs }, Cmd.none )

                Err _ ->
                    ( { model | comments = FailureC }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        column [ padding 12, spacing 12, width fill ]
            [ el [ size 24 ] <| text "Amazing Reddit Clone"
            , button [ padding 12 ]
                { onPress = Just ReloadTopics
                , label = text "Refresh Topics"
                }
            , row [ width fill, spaceEvenly ]
                [ el [ width fill, alignTop ] (viewTopics model.topics)
                , el [ width fill, alignTop ] (viewComments model.comments)
                ]
            ]


viewTopics : Status (List String) -> Element Msg
viewTopics topics =
    let
        topicEl t =
            Input.button [] { onPress = Just (SelectTopic t), label = text t }
    in
    case topics of
        Failure ->
            el [] <| text "Failed to load topics"

        Loading ->
            text "Loading..."

        Success [] ->
            text "No topics available"

        Success ts ->
            column [ spacing 12 ] (List.map topicEl ts)


viewComments : CommentStatus (List Comment) -> Element Msg
viewComments comments =
    case comments of
        FailureC ->
            el [] <| text "Failed to load comments"

        LoadingC ->
            text "Loading..."

        NotLoadedC ->
            text ""

        SuccessC [] ->
            text "No comments available"

        SuccessC cs ->
            column [ spacing 12, width fill ] (List.map viewComment cs)


viewComment : Comment -> Element Msg
viewComment c =
    column [ Border.widthEach { edges | bottom = 1 }, width fill, paddingEach { edges | bottom = 12 } ]
        [ el [] <| text c.text
        , row [ spacing 12 ]
            [ text <| String.fromInt c.id
            , text c.time
            ]
        ]


edges =
    { top = 0
    , right = 0
    , left = 0
    , bottom = 0
    }



-- HTTP


getTopics : Cmd Msg
getTopics =
    Http.get
        { url = "/api/list"
        , expect = Http.expectJson GotTopics topicDecoder
        }


getComments : String -> Cmd Msg
getComments t =
    Http.get
        { url = "/api/" ++ t ++ "/view"
        , expect = Http.expectJson GotComments commentsDecoder
        }



-- JSON decoders


topicDecoder : Decoder (List String)
topicDecoder =
    Json.Decode.list string


commentsDecoder : Decoder (List Comment)
commentsDecoder =
    Json.Decode.list commentDecoder


commentDecoder : Decoder Comment
commentDecoder =
    map4 Comment
        (field "id" int)
        (field "topic" string)
        (field "text" string)
        (field "time" string)
